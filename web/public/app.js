import {
  buildSystem, categorical, sequential, divergent, slate, greyHex, hexToLab, normaliseHex,
  exportObject, snippet, LANGS, CAT_OPTIONS, RAMP_OPTIONS, ENGINE_VERSION,
} from "./engine.js";
import { PALETTES } from "./palettes.js";

const $ = (id) => document.getElementById(id);
const DEFAULT = PALETTES.find((p) => p.name === "metric_trails").colours;
const N_RANGE = { cat: [2, 12, 1], seq: [3, 15, 1], div: [5, 15, 2] };

const state = {
  colours: [...DEFAULT], pivot: true, type: "cat", grey: false, lang: "R",
  cat: { o: "A", n: 5 }, seq: { o: "A", n: 9 }, div: { o: "A", n: 11, c: "white" },
};
let sys = null;
let menuOpen = -1;

// ---------- URL state (shareable, reproducible) ----------
function toHash() {
  const p = new URLSearchParams();
  p.set("c", state.colours.map((h) => h.slice(1)).join("-"));
  p.set("cat", state.cat.o + state.cat.n);
  p.set("seq", state.seq.o + state.seq.n);
  p.set("div", state.div.o + state.div.n + state.div.c[0]);
  if (!state.pivot) p.set("pv", "0");
  return p.toString();
}
function fromHash() {
  const p = new URLSearchParams(location.hash.slice(1));
  const c = (p.get("c") || "").split("-").map((h) => normaliseHex(h)).filter(Boolean);
  if (c.length) state.colours = c;
  const opt = (v, opts) => (v && opts.includes(v[0]) ? v[0] : null);
  const num = (v, [lo, hi]) => { const n = parseInt((v || "").slice(1), 10); return n >= lo && n <= hi ? n : null; };
  const cat = p.get("cat"), seq = p.get("seq"), div = p.get("div");
  if (opt(cat, CAT_OPTIONS)) state.cat.o = cat[0];
  if (num(cat, N_RANGE.cat)) state.cat.n = num(cat, N_RANGE.cat);
  if (opt(seq, RAMP_OPTIONS)) state.seq.o = seq[0];
  if (num(seq, N_RANGE.seq)) state.seq.n = num(seq, N_RANGE.seq);
  if (opt(div, RAMP_OPTIONS)) state.div.o = div[0];
  if (div) { const n = parseInt(div.slice(1), 10); if (n >= 5 && n <= 15 && n % 2) state.div.n = n; if (div.endsWith("b")) state.div.c = "black"; }
  if (p.get("pv") === "0") state.pivot = false;
}
const shareUrl = () => `${location.origin}${location.pathname}#${toHash()}`;

// ---------- colour input (hex, short hex, CSS names, lists) ----------
const ctx = document.createElement("canvas").getContext("2d");
function parseColour(s) {
  const h = normaliseHex(s);
  if (h) return h;
  ctx.fillStyle = "#010203";
  ctx.fillStyle = s.trim();
  const v = ctx.fillStyle;
  if (v === "#010203" && s.trim().toLowerCase() !== "#010203") return null;
  return normaliseHex(v);
}

// ---------- helpers ----------
const ink = (hex) => (hexToLab(hex).L > 0.62 ? sys.black.hex : sys.white.hex);
const shown = (hex) => (state.grey ? greyHex(hex) : hex);
function strip(el, cols, opts = {}) {
  el.innerHTML = "";
  cols.forEach((h, i) => {
    const s = document.createElement("span");
    s.style.background = h;
    if (opts.anchorIndex === i) { s.className = "anc"; s.style.setProperty("--dot", ink(h)); }
    if (opts.labels && cols.length <= (innerWidth < 600 ? 6 : 11)) { s.textContent = h.slice(1); s.style.color = ink(h); }
    s.title = h;
    el.appendChild(s);
  });
}
const el = (tag, attrs = {}, txt) => {
  const e = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (txt != null) e.textContent = txt;
  return e;
};

// ---------- render ----------
function render() {
  try { sys = buildSystem(state.colours, { pivot: state.pivot }); }
  catch (e) { state.colours = [...DEFAULT]; sys = buildSystem(state.colours); }
  const root = document.documentElement.style;
  root.setProperty("--bg", sys.white.hex);
  root.setProperty("--fg", sys.black.hex);
  root.setProperty("--accent", sys.raw5[0].hex);
  root.setProperty("--line", slate(sys));
  document.querySelector('meta[name="theme-color"]')?.setAttribute("content", sys.white.hex);

  const raw7 = [sys.black.hex, ...sys.raw5.map((p) => p.hex), sys.white.hex];
  strip($("hero"), raw7);
  renderChips();
  strip($("stripIn"), sys.input, { anchorIndex: sys.input.indexOf(sys.raw5[0].hex) });
  strip($("strip7"), raw7, { anchorIndex: 1 });
  strip($("stripGrey"), raw7.map(greyHex), { anchorIndex: 1 });
  renderLadder();
  $("log").innerHTML = "";
  sys.log.forEach((l) => {
    const li = document.createElement("li");
    li.innerHTML = `<b>${l.law}</b><span></span>`;
    li.lastChild.textContent = l.text;
    $("log").appendChild(li);
  });
  $("pivot").checked = state.pivot;
  renderSystem();
  renderExport();
  history.replaceState(null, "", "#" + toHash());
}

function renderChips() {
  const box = $("chips");
  box.innerHTML = "";
  const poles = new Set([sys.black.source, sys.white.source]);
  state.colours.forEach((h, i) => {
    const b = document.createElement("button");
    b.type = "button";
    b.className = "chip" + (h === sys.raw5[0].hex && sys.raw5[0].origin === "input" ? " anchor" : "") + (poles.has(h) ? " pole" : "");
    b.innerHTML = `<i style="background:${h}"></i>${h}`;
    if (poles.has(h)) b.title = "Used as a pole";
    b.onclick = (ev) => { ev.stopPropagation(); menuOpen = menuOpen === i ? -1 : i; renderChips(); };
    if (menuOpen === i) {
      const m = document.createElement("div");
      m.className = "chipMenu";
      m.onclick = (ev) => ev.stopPropagation();
      if (i > 0) m.appendChild(menuBtn("Make anchor", () => { state.colours.unshift(...state.colours.splice(i, 1)); }));
      if (state.colours.length > 1) m.appendChild(menuBtn("Remove", () => { state.colours.splice(i, 1); }));
      m.appendChild(menuBtn("Close", () => {}));
      box.appendChild(b);
      box.appendChild(m);
      return;
    }
    box.appendChild(b);
  });
}
function menuBtn(label, fn) {
  const b = document.createElement("button");
  b.type = "button"; b.textContent = label;
  b.onclick = (ev) => { ev.stopPropagation(); fn(); menuOpen = -1; render(); };
  return b;
}

function renderLadder() {
  const svg = $("ladder");
  svg.innerHTML = "";
  const W = Math.max(280, Math.round(svg.getBoundingClientRect().width) || 320), x = (L) => 8 + L * (W - 16);
  svg.setAttribute("viewBox", `0 0 ${W} 64`);
  svg.appendChild(el("rect", { x: 8, y: 26, width: W - 16, height: 4, rx: 2, fill: slate(sys) }));
  [0, 0.25, 0.5, 0.75, 1].forEach((t) => svg.appendChild(el("text", { x: x(t), y: 54, "text-anchor": "middle", "font-size": 8, fill: "currentColor", opacity: 0.6 }, t.toFixed(2))));
  const pts = [
    { hex: sys.black.hex, L: sys.black.L, pole: true },
    ...sys.raw5.map((p, i) => ({ hex: p.hex, L: p.L, anchor: i === 0 })),
    { hex: sys.white.hex, L: sys.white.L, pole: true },
  ];
  pts.forEach((p) => {
    const r = p.anchor ? 9 : 7;
    svg.appendChild(el("circle", { cx: x(p.L), cy: 28, r, fill: p.hex, stroke: sys.black.hex, "stroke-width": p.anchor ? 2.5 : 1 }));
  });
  svg.appendChild(el("text", { x: x(sys.raw5[0].L), y: 12, "text-anchor": "middle", "font-size": 9, "font-weight": 700, fill: "currentColor" }, "anchor"));
  $("gapLbl").textContent = `minimum gap ${sys.minGap.toFixed(3)}`;
}

function currentColours() {
  const t = state.type;
  if (t === "cat") return categorical(sys, state.cat.o, state.cat.n, state.grey);
  if (t === "seq") return sequential(sys, state.seq.o, state.seq.n, state.grey);
  return divergent(sys, state.div.o, state.div.n, state.div.c, state.grey);
}

function renderSystem() {
  const t = state.type, cfg = state[t];
  document.querySelectorAll("#typeSeg button").forEach((b) => b.classList.toggle("on", b.dataset.type === t));
  const pills = $("optPills");
  pills.innerHTML = "";
  (t === "cat" ? CAT_OPTIONS : RAMP_OPTIONS).forEach((o, i) => {
    const b = document.createElement("button");
    b.type = "button"; b.textContent = o; b.className = cfg.o === o ? "on" : "";
    if (t !== "cat") { b.style.boxShadow = `inset 0 -5px 0 ${shown(sys.raw5[i].hex)}`; b.title = `Built on core colour ${i + 1}`; }
    b.onclick = () => { cfg.o = o; render(); };
    pills.appendChild(b);
  });
  $("nVal").textContent = `${cfg.n} ${t === "cat" ? "colours" : "stops"}`;
  $("centreSeg").style.display = t === "div" ? "" : "none";
  document.querySelectorAll("#centreSeg button").forEach((b) => b.classList.toggle("on", b.dataset.c === state.div.c));
  $("greyscale").checked = state.grey;
  const cols = currentColours();
  strip($("stripSys"), cols, { labels: true, anchorIndex: t === "cat" && cfg.o !== "D" ? 0 : -1 });
  renderChart(cols);
}

// ---------- sample charts, themed by the system itself ----------
function renderChart(cols) {
  const W = 640, H = 300, P = { l: 40, r: 14, t: 18, b: 30 };
  const bg = sys.white.hex, fg = sys.black.hex, grid = slate(sys);
  const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, role: "img", "aria-label": "Sample chart" });
  svg.appendChild(el("rect", { width: W, height: H, fill: bg }));
  const font = { "font-family": "DM Sans, sans-serif", "font-size": 12, fill: fg };
  const iw = W - P.l - P.r, ih = H - P.t - P.b;

  if (state.type === "cat") {
    const vals = cols.map((_, i) => 0.35 + 0.6 * Math.abs(Math.sin(i * 1.7 + 0.6)));
    [0, 0.25, 0.5, 0.75, 1].forEach((g) => {
      const y = P.t + ih * (1 - g);
      svg.appendChild(el("line", { x1: P.l, x2: W - P.r, y1: y, y2: y, stroke: grid, "stroke-width": 1 }));
      svg.appendChild(el("text", { ...font, x: P.l - 8, y: y + 4, "text-anchor": "end", "font-size": 10 }, (g * 100).toFixed(0)));
    });
    const bw = iw / cols.length;
    cols.forEach((c, i) => {
      const h = vals[i] * ih;
      svg.appendChild(el("rect", { x: P.l + i * bw + bw * 0.14, y: P.t + ih - h, width: bw * 0.72, height: h, rx: 3, fill: c }));
      svg.appendChild(el("text", { ...font, x: P.l + i * bw + bw / 2, y: H - 10, "text-anchor": "middle", "font-size": 10 }, String.fromCharCode(65 + i)));
    });
  } else if (state.type === "seq") {
    const nx = 32, ny = 14, cw = iw / nx, ch = ih / ny;
    for (let j = 0; j < ny; j++) for (let i = 0; i < nx; i++) {
      const z = 0.5 + 0.5 * Math.sin(i / 5) * Math.cos(j / 3.2 + i / 11);
      const k = Math.min(cols.length - 1, Math.floor((1 - z) * cols.length));
      svg.appendChild(el("rect", { x: P.l + i * cw, y: P.t + j * ch, width: cw + 0.5, height: ch + 0.5, fill: cols[k] }));
    }
    svg.appendChild(el("text", { ...font, x: P.l, y: H - 10, "font-size": 10 }, "high"));
    svg.appendChild(el("text", { ...font, x: W - P.r, y: H - 10, "text-anchor": "end", "font-size": 10 }, `${cols.length} classes`));
  } else {
    const m = 15, vals = Array.from({ length: m }, (_, i) => Math.sin(i * 0.9 + 0.4) * (0.4 + (i % 4) * 0.2));
    const bh = ih / m, x0 = P.l + iw / 2;
    svg.appendChild(el("line", { x1: x0, x2: x0, y1: P.t, y2: P.t + ih, stroke: fg, "stroke-width": 1 }));
    vals.forEach((v, i) => {
      const k = Math.round(((v + 1) / 2) * (cols.length - 1));
      const w = Math.abs(v) * (iw / 2) * 0.95;
      svg.appendChild(el("rect", { x: v < 0 ? x0 - w : x0, y: P.t + i * bh + bh * 0.15, width: w, height: bh * 0.7, rx: 2, fill: cols[k] }));
    });
    svg.appendChild(el("text", { ...font, x: P.l, y: H - 10, "font-size": 10 }, "−"));
    svg.appendChild(el("text", { ...font, x: W - P.r, y: H - 10, "text-anchor": "end", "font-size": 10 }, "+"));
    svg.appendChild(el("text", { ...font, x: x0, y: H - 10, "text-anchor": "middle", "font-size": 10 }, `${state.div.c} centre`));
  }
  $("chart").innerHTML = "";
  $("chart").appendChild(svg);
}

function renderExport() {
  const seg = $("langSeg");
  seg.innerHTML = "";
  LANGS.forEach((l) => {
    const b = document.createElement("button");
    b.type = "button"; b.textContent = l; b.className = l === state.lang ? "on" : "";
    b.onclick = () => { state.lang = l; renderExport(); };
    seg.appendChild(b);
  });
  const obj = exportObject(sys, {
    catOption: state.cat.o, catN: state.cat.n, seqOption: state.seq.o, seqN: state.seq.n,
    divOption: state.div.o, divN: state.div.n, divCentre: state.div.c,
  });
  $("code").textContent = snippet(state.lang, obj, shareUrl());
}

// ---------- events ----------
$("addForm").onsubmit = (e) => {
  e.preventDefault();
  const raw = $("hexIn").value.trim();
  const parts = raw ? raw.split(/[\s,;]+/).filter(Boolean) : [$("picker").value];
  const good = parts.map(parseColour).filter(Boolean);
  if (!good.length) { $("hexIn").setCustomValidity("Not a colour"); $("hexIn").reportValidity(); return; }
  $("hexIn").setCustomValidity("");
  good.forEach((h) => { if (!state.colours.includes(h)) state.colours.push(h); });
  $("hexIn").value = "";
  render();
};
$("hexIn").oninput = () => $("hexIn").setCustomValidity("");
$("picker").onchange = () => { $("hexIn").value = $("picker").value.toUpperCase(); };
$("pivot").onchange = (e) => { state.pivot = e.target.checked; render(); };
$("greyscale").onchange = (e) => { state.grey = e.target.checked; renderSystem(); };
document.querySelectorAll("#typeSeg button").forEach((b) => (b.onclick = () => { state.type = b.dataset.type; renderSystem(); }));
document.querySelectorAll("#centreSeg button").forEach((b) => (b.onclick = () => { state.div.c = b.dataset.c; render(); }));
document.querySelectorAll("#nCtl button").forEach((b) => (b.onclick = () => {
  const t = state.type, [lo, hi, step] = N_RANGE[t];
  state[t].n = Math.min(hi, Math.max(lo, state[t].n + step * Number(b.dataset.d)));
  render();
}));
$("copy").onclick = async () => {
  try { await navigator.clipboard.writeText($("code").textContent); $("copy").textContent = "Copied"; }
  catch { $("copy").textContent = "Select and copy"; }
  setTimeout(() => ($("copy").textContent = "Copy"), 1400);
};
document.addEventListener("click", () => { if (menuOpen !== -1) { menuOpen = -1; renderChips(); } });
window.addEventListener("resize", () => sys && renderLadder());
window.addEventListener("hashchange", () => { fromHash(); render(); });

// presets
PALETTES.forEach((p) => {
  const b = document.createElement("button");
  b.type = "button"; b.className = "preset";
  const bar = document.createElement("div");
  bar.className = "raw7";
  p.colours.forEach((c) => { const s = document.createElement("span"); s.style.background = c; bar.appendChild(s); });
  b.appendChild(bar);
  b.appendChild(document.createTextNode(p.name.replace(/_/g, " ")));
  b.onclick = () => { state.colours = [...p.colours]; render(); $("input").scrollIntoView({ behavior: "smooth" }); };
  $("presets").appendChild(b);
});

$("ver").textContent = `Engine ${ENGINE_VERSION}`;
fromHash();
render();
