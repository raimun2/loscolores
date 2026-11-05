// loscolores engine (web). Same spirit as the R package, rebuilt from its laws,
// not ported line by line. Pure functions, deterministic, no dependencies.
//
// Laws:
//  1. Rules over taste: every output obeys the same perceptual laws.
//  2. The anchor (first input) is sacred: exact hex, exact L, always slot 1.
//  3. Luminance is the backbone: 7 levels (2 poles + 5 core) spread in L so the
//     system reads in greyscale. Gamut mapping never touches L, only chroma.
//  4. Scaffolding first: poles define the horizon and are always neutral greys.
//     Only an achromatic input near a pole can become that pole.
//  5. Any input resolves to the same skeleton: distil (>5) or expand (<5).
//  6. Pathological inputs have explicit exits, never a collapse.
//  7. One core, many uses: categorical, sequential, divergent, theme.
//  8. Transparent: every decision is logged.

export const ENGINE_VERSION = "1.1.1";

// Reference tones for the default poles. Only their L is used: poles are always
// neutral, so the actual default poles are greys at these lightnesses.
export const DEFAULT_BLACK = "#1D1C1B";
export const DEFAULT_WHITE = "#F4F3F3";

const POLE_ZONE = 0.08;     // L distance from a default pole that counts as "pole territory"
const POLE_CHROMA = 0.04;   // above this chroma a colour is a colour, never a pole
const VOID_CHROMA = 0.02;   // below this chroma a colour has no usable hue
const VOID_HUE = 240;       // hue seed when the anchor is achromatic
const VOID_SEED_C = 0.12;   // chroma seed when the anchor is achromatic
const JAM_VARIANCE = 0.05;  // hue circular variance below this is a traffic jam
const PIVOT_DEG = 40;

// ---------------------------------------------------------------------------
// Colour maths (OKLab, Björn Ottosson)
// ---------------------------------------------------------------------------
const toLin = (v) => (v <= 0.04045 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4));
const toGam = (v) => (v <= 0.0031308 ? 12.92 * v : 1.055 * Math.pow(v, 1 / 2.4) - 0.055);

// Plain arithmetic (no Math.hypot / Math.cbrt) so the R package can reproduce
// the same doubles operation by operation.
const cbrt = (x) => Math.pow(x, 1 / 3);
const hyp2 = (x, y) => Math.sqrt(x * x + y * y);

export function hexToRgb(hex) {
  const h = hex.replace("#", "");
  return [0, 2, 4].map((i) => parseInt(h.slice(i, i + 2), 16) / 255);
}

export function rgbToHex([r, g, b]) {
  const c = (v) => Math.round(Math.min(1, Math.max(0, v)) * 255).toString(16).padStart(2, "0");
  return ("#" + c(r) + c(g) + c(b)).toUpperCase();
}

export function rgbToOklab([r, g, b]) {
  const R = toLin(r), G = toLin(g), B = toLin(b);
  const l = cbrt(0.4122214708 * R + 0.5363325363 * G + 0.0514459929 * B);
  const m = cbrt(0.2119034982 * R + 0.6806995451 * G + 0.1073969566 * B);
  const s = cbrt(0.0883024619 * R + 0.2817188376 * G + 0.6299787005 * B);
  return {
    L: 0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
    a: 1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
    b: 0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s,
  };
}

export function oklabToLinear({ L, a, b }) {
  const l = (L + 0.3963377774 * a + 0.2158037573 * b) ** 3;
  const m = (L - 0.1055613458 * a - 0.0638541728 * b) ** 3;
  const s = (L - 0.0894841775 * a - 1.291485548 * b) ** 3;
  return [
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s,
  ];
}

export const hexToLab = (hex) => withPolar(rgbToOklab(hexToRgb(hex)));

export function withPolar(c) {
  const C = hyp2(c.a, c.b);
  const h = ((Math.atan2(c.b, c.a) * 180) / Math.PI + 360) % 360;
  return { L: c.L, a: c.a, b: c.b, c: C, h };
}

export const fromLCh = (L, C, h) =>
  withPolar({ L, a: C * Math.cos((h * Math.PI) / 180), b: C * Math.sin((h * Math.PI) / 180) });

const inGamut = (lin, eps = 1e-6) => lin.every((v) => v >= -eps && v <= 1 + eps);

// Gamut mapping that keeps L and hue: shrink chroma until the colour fits sRGB.
export function mapToGamut(col) {
  const L = Math.min(1, Math.max(0, col.L));
  if (inGamut(oklabToLinear({ L, a: col.a, b: col.b }))) return { col: withPolar({ L, a: col.a, b: col.b }), clipped: false };
  let lo = 0, hi = col.c;
  for (let i = 0; i < 30; i++) {
    const mid = (lo + hi) / 2;
    if (inGamut(oklabToLinear(fromLCh(L, mid, col.h)))) lo = mid; else hi = mid;
  }
  return { col: fromLCh(L, lo, col.h), clipped: true };
}

export function labToHex(col) {
  const { col: g } = mapToGamut(col);
  return rgbToHex(oklabToLinear(g).map(toGam));
}

const grey = (L) => ({ L, a: 0, b: 0, c: 0, h: 0 });
const dist = (p, q) => Math.sqrt((p.L - q.L) * (p.L - q.L) + (p.a - q.a) * (p.a - q.a) + (p.b - q.b) * (p.b - q.b));
const hueGap = (x, y) => { const d = Math.abs(x - y) % 360; return Math.min(d, 360 - d); };

function circularVariance(hues) {
  if (hues.length < 2) return 1;
  const r = hues.map((h) => (h * Math.PI) / 180);
  const C = r.reduce((s, v) => s + Math.cos(v), 0) / r.length;
  const S = r.reduce((s, v) => s + Math.sin(v), 0) / r.length;
  return 1 - hyp2(C, S);
}

// ---------------------------------------------------------------------------
// Input customs
// ---------------------------------------------------------------------------
export function normaliseHex(s) {
  if (typeof s !== "string") return null;
  let h = s.trim().replace(/^#/, "");
  if (/^[0-9a-f]{3}$/i.test(h)) h = h.split("").map((x) => x + x).join("");
  if (/^[0-9a-f]{8}$/i.test(h)) h = h.slice(0, 6);
  return /^[0-9a-f]{6}$/i.test(h) ? "#" + h.toUpperCase() : null;
}

// ---------------------------------------------------------------------------
// Distillation: deterministic k-means in OKLab, returns real input colours
// (medoids), so the satellites are colours the user actually brought.
// ---------------------------------------------------------------------------
// Near-ties (evenly spaced ramps make exact ties) go to the first candidate, so
// float noise in the last bits never changes the outcome across engines.
const TIE = 1e-9;

function distil(points, k, anchor) {
  // farthest-point init, seeded by distance to the anchor
  const centres = [];
  let first = 0, best = -1;
  points.forEach((p, i) => { const d = dist(p, anchor); if (d > best + TIE) { best = d; first = i; } });
  centres.push({ ...points[first] });
  while (centres.length < k) {
    let idx = 0, far = -1;
    points.forEach((p, i) => {
      const d = Math.min(dist(p, anchor), ...centres.map((c) => dist(p, c)));
      if (d > far + TIE) { far = d; idx = i; }
    });
    centres.push({ ...points[idx] });
  }
  let assign = [];
  for (let it = 0; it < 25; it++) {
    assign = points.map((p) => centres.reduce((bi, c, i) => (dist(p, c) < dist(p, centres[bi]) - TIE ? i : bi), 0));
    centres.forEach((c, i) => {
      const mem = points.filter((_, j) => assign[j] === i);
      if (mem.length) {
        c.L = mem.reduce((s, p) => s + p.L, 0) / mem.length;
        c.a = mem.reduce((s, p) => s + p.a, 0) / mem.length;
        c.b = mem.reduce((s, p) => s + p.b, 0) / mem.length;
      }
    });
  }
  // medoid: the real colour closest to each centre, no repeats
  const used = new Set();
  return centres.map((c) => {
    let bi = -1, bd = Infinity;
    points.forEach((p, i) => { if (!used.has(i) && dist(p, c) < bd - TIE) { bd = dist(p, c); bi = i; } });
    used.add(bi);
    return points[bi];
  });
}

// Greedy hue placement: each new hue sits as far as possible from the others.
function spreadHues(existing, n, start) {
  const hues = [...existing];
  const out = [];
  for (let i = 0; i < n; i++) {
    if (!hues.length) { hues.push(start); out.push(start); continue; }
    let bestH = 0, bestD = -1;
    for (let h = 0; h < 360; h += 1) {
      const d = Math.min(...hues.map((x) => hueGap(x, h)));
      // tie-break: closest to start + 180 keeps results stable
      if (d > bestD + 1e-9) { bestD = d; bestH = h; }
    }
    hues.push(bestH); out.push(bestH);
  }
  return out;
}

// ---------------------------------------------------------------------------
// The core: 2 poles + 5 colours
// ---------------------------------------------------------------------------
export function buildSystem(inputHex, opts = {}) {
  const pivot = opts.pivot !== false;
  const log = [];
  const input = [...new Set(inputHex.map(normaliseHex).filter(Boolean))];
  if (!input.length) throw new Error("No valid colour in the input.");
  const lab = input.map((hex) => ({ hex, ...hexToLab(hex) }));

  // -- 1. Poles (the horizon) -------------------------------------------------
  const Lb0 = hexToLab(DEFAULT_BLACK).L, Lw0 = hexToLab(DEFAULT_WHITE).L;
  let black = { ...grey(Lb0), hex: labToHex(grey(Lb0)), source: "default" };
  let white = { ...grey(Lw0), hex: labToHex(grey(Lw0)), source: "default" };
  const signal = [];
  for (const p of lab) {
    const achromatic = p.c < POLE_CHROMA;
    if (achromatic && p.L <= Lb0 + POLE_ZONE && black.source === "default") {
      black = { ...grey(p.L), hex: labToHex(grey(p.L)), source: p.hex };
      log.push({ law: "pole", text: `${p.hex} is near-black and neutral: it becomes the dark pole.` });
    } else if (achromatic && p.L >= Lw0 - POLE_ZONE && white.source === "default") {
      white = { ...grey(p.L), hex: labToHex(grey(p.L)), source: p.hex };
      log.push({ law: "pole", text: `${p.hex} is near-white and neutral: it becomes the light pole.` });
    } else if (achromatic && (p.L <= Lb0 + POLE_ZONE || p.L >= Lw0 - POLE_ZONE)) {
      log.push({ law: "pole", text: `${p.hex} sits in a pole zone already taken: dropped.` });
    } else {
      signal.push(p);
    }
  }
  for (const p of lab) {
    if (p.c >= POLE_CHROMA && (p.L <= Lb0 + POLE_ZONE || p.L >= Lw0 - POLE_ZONE))
      log.push({ law: "pole", text: `${p.hex} is very ${p.L < 0.5 ? "dark" : "light"} but has colour (chroma ${p.c.toFixed(2)}): kept as a colour, not a pole.` });
  }

  // -- 2. Anchor ----------------------------------------------------------------
  let anchor;
  if (signal.length) {
    anchor = { ...signal[0], role: "anchor", origin: "input" };
    if (signal[0].hex !== input[0])
      log.push({ law: "anchor", text: `${input[0]} became a pole, so ${anchor.hex} is the anchor.` });
  } else {
    const L = (black.L + white.L) / 2;
    anchor = { ...grey(L), hex: labToHex(grey(L)), role: "anchor", origin: "synthetic" };
    log.push({ law: "anchor", text: `The input is only poles: a neutral mid-grey ${anchor.hex} anchors the system.` });
  }

  // -- 3. Distil or expand to 4 satellites -----------------------------------------
  let sats = signal.slice(1).map((p) => ({ ...p, origin: "input" }));
  const nIn = signal.length;
  if (sats.length > 4) {
    sats = distil(sats, 4, anchor).map((p) => ({ ...p, origin: "distilled" }));
    log.push({ law: "distil", text: `${nIn} colours distilled to 5: the anchor plus 4 cluster representatives (real input colours).` });
  }
  if (sats.length < 4) {
    const missing = 4 - sats.length;
    const chromatic = [anchor, ...sats].filter((p) => p.c >= VOID_CHROMA);
    const seedC = chromatic.length ? chromatic.reduce((s, p) => s + p.c, 0) / chromatic.length : VOID_SEED_C;
    const startHue = anchor.c >= VOID_CHROMA ? (anchor.h + 180) % 360 : VOID_HUE;
    const hues = spreadHues(chromatic.map((p) => p.h), missing, startHue);
    hues.forEach((h) => sats.push({ ...fromLCh(0.5, seedC, h), origin: "expanded" }));
    log.push({ law: "expand", text: `${nIn || 1} colour${nIn === 1 ? "" : "s"} expanded to 5: ${missing} new hue${missing === 1 ? "" : "s"} placed as far as possible from the others (chroma ${seedC.toFixed(2)}).` });
  }

  // -- 4. Hue pivot (traffic jam) ----------------------------------------------------
  const chromaHues = [anchor, ...sats].filter((p) => p.c >= VOID_CHROMA).map((p) => p.h);
  const cv = circularVariance(chromaHues);
  sats.sort((p, q) => p.L - q.L);
  if (chromaHues.length >= 3 && cv < JAM_VARIANCE) {
    if (pivot) {
      [[1, PIVOT_DEG], [3, -PIVOT_DEG]].forEach(([i, d]) => {
        const s = sats[i];
        if (s.c >= VOID_CHROMA) sats[i] = { ...fromLCh(s.L, s.c, (s.h + d + 360) % 360), origin: s.origin, pivoted: d };
      });
      log.push({ law: "pivot", text: `Hues are jammed (circular variance ${cv.toFixed(3)}): two satellites rotated ±${PIVOT_DEG}°.` });
    } else {
      log.push({ law: "pivot", text: `Hues are jammed (circular variance ${cv.toFixed(3)}) and the pivot is off: monochrome system.` });
    }
  }

  // -- 5. L ladder (luminance is the backbone) -------------------------------------
  const target = (Lw0 - Lb0) / 6;
  const La = anchor.L;
  if (white.L - La < target) {
    const nl = Math.min(1, La + target);
    if (nl > white.L) {
      white = { ...grey(nl), hex: labToHex(grey(nl)), source: white.source };
      log.push({ law: "ladder", text: `The anchor is very light: the light pole moves up to L ${nl.toFixed(2)} to make room.` });
    }
  }
  if (La - black.L < target) {
    const nl = Math.max(0, La - target);
    if (nl < black.L) {
      black = { ...grey(nl), hex: labToHex(grey(nl)), source: black.source };
      log.push({ law: "ladder", text: `The anchor is very dark: the dark pole moves down to L ${nl.toFixed(2)} to make room.` });
    }
  }
  let bestK = 0, bestMin = -1;
  for (let k = 0; k <= 4; k++) {
    const gb = (La - black.L) / (k + 1), ga = (white.L - La) / (5 - k);
    const m = Math.min(gb, ga);
    if (m > bestMin + 1e-12) { bestMin = m; bestK = k; }
  }
  const gb = (La - black.L) / (bestK + 1), ga = (white.L - La) / (5 - bestK);
  const slots = [];
  for (let j = 1; j <= bestK; j++) slots.push(black.L + j * gb);
  for (let j = 1; j <= 4 - bestK; j++) slots.push(La + j * ga);
  let clipped = 0;
  const core = sats.map((s, i) => {
    const want = s.c < VOID_CHROMA ? grey(slots[i]) : fromLCh(slots[i], s.c, s.h);
    const m = mapToGamut(want);
    if (m.clipped) clipped++;
    return { ...m.col, hex: labToHex(m.col), origin: s.origin, from: s.hex || null, wantC: s.c };
  });
  log.push({ law: "ladder", text: `L ladder: ${bestK} satellite${bestK === 1 ? "" : "s"} below the anchor and ${4 - bestK} above. Minimum gap ${bestMin.toFixed(3)}.` });
  if (clipped) log.push({ law: "gamut", text: `${clipped} satellite${clipped === 1 ? "" : "s"} lost chroma to fit sRGB at their L (L and hue kept).` });

  const anchorCol = { ...withPolar(anchor), hex: anchor.hex, origin: anchor.origin, role: "anchor" };
  const raw5 = [anchorCol, ...core];
  const levels = [black.L, ...raw5.map((p) => p.L), white.L].sort((x, y) => x - y);
  const minGap = Math.min(...levels.slice(1).map((v, i) => v - levels[i]));

  return {
    engine: ENGINE_VERSION,
    input,
    black: { ...withPolar(black), hex: black.hex, source: black.source },
    white: { ...withPolar(white), hex: white.hex, source: white.source },
    raw5,
    minGap,
    hueVariance: cv,
    log,
  };
}

// ---------------------------------------------------------------------------
// Expansions (one core, many uses)
// ---------------------------------------------------------------------------
export const CAT_OPTIONS = ["A", "B", "C", "D"];
export const RAMP_OPTIONS = ["A", "B", "C", "D", "E"];
const CAT_ORDER = { A: [0, 1, 2, 3, 4], B: [0, 3, 1, 4, 2], C: [0, 4, 1, 3, 2], D: [0, 1, 2, 3, 4] };

const lerp = (p, q, t) => withPolar({ L: p.L + (q.L - p.L) * t, a: p.a + (q.a - p.a) * t, b: p.b + (q.b - p.b) * t });
const greyOut = (cols, g) => (g ? cols.map((c) => grey(c.L)) : cols);

export function categorical(sys, option = "A", n = 5, greyscale = false) {
  let nodes = CAT_ORDER[option].map((i) => sys.raw5[i]);
  if (option === "D") nodes = nodes.map((p, i) => (i === 0 || p.c < 0.01 ? p : fromLCh(p.L, Math.max(p.c, 0.15), (p.h + 15) % 360)));
  let cols;
  if (n <= 5) cols = nodes.slice(0, n);
  else cols = Array.from({ length: n }, (_, i) => {
    const t = (i / (n - 1)) * 4, k = Math.min(3, Math.floor(t));
    return lerp(nodes[k], nodes[k + 1], t - k);
  });
  const hex = greyOut(cols, greyscale).map(labToHex);
  if (!greyscale && option !== "D") hex[0] = sys.raw5[0].hex; // the anchor stays exact
  return hex;
}

export function sequential(sys, option = "A", n = 9, greyscale = false) {
  const base = sys.raw5[RAMP_OPTIONS.indexOf(option)];
  const end = withPolar({ L: sys.white.L * 0.97, a: base.a * 0.04, b: base.b * 0.04 });
  return greyOut(Array.from({ length: n }, (_, i) => lerp(base, end, i / (n - 1))), greyscale).map(labToHex);
}

// Divergent: the base and its complement at the same L and chroma, so both arms
// weigh the same in greyscale; centre is a pole.
export function divergent(sys, option = "A", n = 11, centre = "white", greyscale = false) {
  const high = sys.raw5[RAMP_OPTIONS.indexOf(option)];
  const low = mapToGamut(fromLCh(high.L, high.c, (high.h + 180) % 360)).col;
  const mid = centre === "black" ? sys.black : sys.white;
  const cols = Array.from({ length: n }, (_, i) => {
    const t = i / (n - 1);
    return t <= 0.5 ? lerp(low, mid, t * 2) : lerp(mid, high, (t - 0.5) * 2);
  });
  return greyOut(cols, greyscale).map(labToHex);
}

export function slate(sys) {
  return labToHex(withPolar({ L: sys.white.L * 0.85 + sys.black.L * 0.15, a: sys.black.a * 0.2, b: sys.black.b * 0.2 }));
}

export const greyHex = (hex) => labToHex(grey(hexToLab(hex).L));

// ---------------------------------------------------------------------------
// Export: the whole system as a one-line object in six languages
// ---------------------------------------------------------------------------
export function exportObject(sys, cfg) {
  return {
    black: sys.black.hex,
    white: sys.white.hex,
    slate: slate(sys),
    core: sys.raw5.map((p) => p.hex),
    cat: categorical(sys, cfg.catOption, cfg.catN),
    seq: sequential(sys, cfg.seqOption, cfg.seqN),
    div: divergent(sys, cfg.divOption, cfg.divN, cfg.divCentre),
  };
}

const KEYS = ["black", "white", "slate", "core", "cat", "seq", "div"];
const q = (s) => `"${s}"`;
const rgbStr = (hex) => hexToRgb(hex).map((v) => Math.round(v * 255)).join(" ");
const pascal = (k) => k[0].toUpperCase() + k.slice(1);

export function snippet(lang, obj, url) {
  const v = (k, fn) => (Array.isArray(obj[k]) ? fn(obj[k]) : q(obj[k]));
  switch (lang) {
    case "R":
      return `# ${url}\nlc <- list(${KEYS.map((k) => `${k} = ${v(k, (a) => `c(${a.map(q).join(", ")})`)}`).join(", ")})`;
    case "Python":
      return `# ${url}\nlc = {${KEYS.map((k) => `"${k}": ${v(k, (a) => `[${a.map(q).join(", ")}]`)}`).join(", ")}}`;
    case "MATLAB":
      return `% ${url}\nlc = struct(${KEYS.map((k) => `'${k}', ${v(k, (a) => `{[${a.map(q).join(" ")}]}`)}`).join(", ")});`;
    case "Stata":
      return `* ${url}\n* Stata takes colours as "R G B" strings; one global per key.\n` +
        KEYS.map((k) => `global lc_${k} ${Array.isArray(obj[k]) ? "`\"" + obj[k].map((h) => `"${rgbStr(h)}"`).join(" ") + "\"'" : `"${rgbStr(obj[k])}"`}`).join("\n");
    case "JS":
      return `// ${url}\nconst lc = {${KEYS.map((k) => `${k}: ${v(k, (a) => `[${a.map(q).join(", ")}]`)}`).join(", ")}};`;
    case "C#":
      return `// ${url}\nvar lc = new { ${KEYS.map((k) => `${pascal(k)} = ${v(k, (a) => `new[] { ${a.map(q).join(", ")} }`)}`).join(", ")} };`;
    default:
      throw new Error("Unknown language " + lang);
  }
}

export const LANGS = ["R", "Python", "MATLAB", "Stata", "JS", "C#"];
