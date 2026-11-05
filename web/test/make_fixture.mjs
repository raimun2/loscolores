// Writes the cross-engine fixture used by the R package tests.
// Run from the repo root after any engine change: node web/test/make_fixture.mjs
import { writeFileSync, mkdirSync } from "node:fs";
import { buildSystem, categorical, sequential, divergent, slate, ENGINE_VERSION } from "../public/engine.js";
import { PALETTES } from "../public/palettes.js";

const EDGE = {
  single_blue: ["#096ED3"], single_red: ["#E63946"], single_yellow: ["#FFE600"],
  near_black: ["#222222"], near_white: ["#EEEEEE"], pure_black: ["#000000"], pure_white: ["#FFFFFF"],
  only_poles: ["#000000", "#FFFFFF"], mid_grey: ["#808080"], greys: ["#333333", "#777777", "#BBBBBB"],
  warm_offwhite_first: ["#FAF3E0", "#096ED3"], navy_anchor: ["#0B1B3F", "#F9A730"],
  same_hue: ["#08306B", "#2171B5", "#6BAED6", "#C6DBEF"], same_L: ["#E63946", "#2A9D8F", "#8A6FDF"],
  many: ["#E63946", "#F1FAEE", "#A8DADC", "#457B9D", "#1D3557", "#FFB703", "#FB8500", "#8ECAE6", "#219EBC"],
  yellow_first: ["#E0C41A", "#096ED3"],
};
const runs = [
  ...PALETTES.map((p) => ({ name: p.name, input: p.colours, pivot: true })),
  ...Object.entries(EDGE).map(([name, input]) => ({ name, input, pivot: true })),
  ...["raimun2_seq_1", "raimun2_seq_13"].map((n) => ({ name: n + "_nopivot", input: PALETTES.find((p) => p.name === n).colours, pivot: false })),
  { name: "same_hue_nopivot", input: EDGE.same_hue, pivot: false },
];

const L = ["A", "B", "C", "D", "E"];
const cases = runs.map(({ name, input, pivot }) => {
  const s = buildSystem(input, { pivot });
  return {
    name, input, pivot,
    black: s.black.hex, white: s.white.hex, slate: slate(s),
    raw5: s.raw5.map((p) => p.hex),
    laws: s.log.map((l) => l.law),
    cat5: Object.fromEntries(L.slice(0, 4).map((o) => [o, categorical(s, o, 5)])),
    cat8: Object.fromEntries(L.slice(0, 4).map((o) => [o, categorical(s, o, 8)])),
    seq9: Object.fromEntries(L.map((o) => [o, sequential(s, o, 9)])),
    div11w: Object.fromEntries(L.map((o) => [o, divergent(s, o, 11, "white")])),
    div11b: Object.fromEntries(L.map((o) => [o, divergent(s, o, 11, "black")])),
    catGrey: categorical(s, "A", 5, true),
  };
});

const out = new URL("../../tests/testthat/fixtures/", import.meta.url);
mkdirSync(out, { recursive: true });
writeFileSync(new URL("engine_cases.json", out), JSON.stringify({ engine: ENGINE_VERSION, cases }, null, 1));
console.log(`fixture: ${cases.length} cases, engine ${ENGINE_VERSION}`);
