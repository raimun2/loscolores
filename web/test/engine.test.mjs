// Run: node web/test/engine.test.mjs
import assert from "node:assert/strict";
import { buildSystem, hexToLab, categorical, sequential, divergent, exportObject, snippet, LANGS } from "../public/engine.js";
import { PALETTES } from "../public/palettes.js";

const EDGE = {
  single_blue: ["#096ED3"], single_red: ["#E63946"], single_yellow: ["#FFE600"],
  near_black: ["#222222"], near_white: ["#EEEEEE"], pure_black: ["#000000"], pure_white: ["#FFFFFF"],
  only_poles: ["#000000", "#FFFFFF"], mid_grey: ["#808080"], greys: ["#333333", "#777777", "#BBBBBB"],
  warm_offwhite_first: ["#FAF3E0", "#096ED3"], navy_anchor: ["#0B1B3F", "#F9A730"],
  same_hue: ["#08306B", "#2171B5", "#6BAED6", "#C6DBEF"], same_L: ["#E63946", "#2A9D8F", "#8A6FDF"],
  many: ["#E63946", "#F1FAEE", "#A8DADC", "#457B9D", "#1D3557", "#FFB703", "#FB8500", "#8ECAE6", "#219EBC"],
  yellow_first: ["#E0C41A", "#096ED3"], bad_input: ["nope", "#12", "#096ED3"],
};
const cases = { ...Object.fromEntries(PALETTES.map((p) => [p.name, p.colours])), ...EDGE };

let rows = [];
for (const [name, cols] of Object.entries(cases)) {
  const s = buildSystem(cols);
  const s2 = buildSystem(cols);
  assert.deepEqual(s.raw5.map((p) => p.hex), s2.raw5.map((p) => p.hex), `${name}: not deterministic`);
  assert.equal(s.raw5.length, 5, `${name}: core is not 5`);
  assert.equal(new Set(s.raw5.map((p) => p.hex)).size, 5, `${name}: duplicate core colours`);
  // the anchor is exact when it is a real input colour
  if (s.raw5[0].origin === "input") assert.ok(s.input.includes(s.raw5[0].hex), `${name}: anchor changed`);
  // L ladder measured on the actual hex output, not on intentions
  const Ls = [s.black.hex, ...s.raw5.map((p) => p.hex), s.white.hex].map((h) => hexToLab(h).L).sort((a, b) => a - b);
  const gap = Math.min(...Ls.slice(1).map((v, i) => v - Ls[i]));
  rows.push([name, s.raw5[0].hex, gap.toFixed(3), s.black.hex, s.white.hex, s.raw5.map((p) => p.hex).join(" ")]);
  assert.ok(gap > 0.05, `${name}: L gap collapsed (${gap.toFixed(3)})`);
  for (const o of ["A", "B", "C", "D"]) assert.equal(categorical(s, o, 8).length, 8);
  assert.equal(sequential(s, "C", 9).length, 9);
  assert.equal(divergent(s, "E", 11, "black").length, 11);
}
// explicit expectations from the R diagnosis
const nav = buildSystem(EDGE.navy_anchor);
assert.equal(nav.raw5[0].hex, "#0B1B3F", "navy anchor must survive (R turned it into the black pole)");
const yel = buildSystem(EDGE.yellow_first);
assert.equal(yel.raw5[0].hex, "#E0C41A", "a light yellow is a colour, not the white pole");
assert.equal(yel.white.source, "default");
const off = buildSystem(EDGE.warm_offwhite_first);
assert.equal(off.white.source, "#FAF3E0", "neutral off-white becomes the light pole");
assert.equal(off.raw5[0].hex, "#096ED3");

console.table(rows.map(([n, a, g, b, w, c]) => ({ case: n, anchor: a, minGap: g, black: b, white: w, core: c })));
const obj = exportObject(buildSystem(PALETTES[0].colours), { catOption: "A", catN: 5, seqOption: "A", seqN: 9, divOption: "A", divN: 11, divCentre: "white" });
import { writeFileSync } from "node:fs";
for (const l of LANGS) writeFileSync(`/tmp/lc_snippet_${l.replace("#", "sharp")}.txt`, snippet(l, obj, "https://loscolores.raimundos.cl/#c=096ED3-F9A730-F99DC2-303F47"));
console.log("OK", Object.keys(cases).length, "cases");
