# loscolores (web)

Artefact of raimundos.cl, served at https://loscolores.raimundos.cl.
This folder is the web tool. The repo root is still the R package, so
`pacman::p_load_gh("raimun2/loscolores")` keeps working. `^web$` is in the root `.Rbuildignore`.

## What it is
A tool that takes any reference colour(s) and builds a perceptual colour system
(OKLab): 2 poles + 5 core colours, plus categorical, sequential and divergent
variants, shown visually with its reasoning, and exported as a one-line object in
R, Python, MATLAB, Stata, JS and C#.

The R package at the repo root runs the SAME engine (R/utils_color.R,
R/engine_core.R, R/engine_expansion.R mirror public/engine.js operation by
operation) and must return identical hex codes. Change both engines together.

## The 8 laws (do not break)
1. Rules over taste: every output obeys the same perceptual laws.
2. The anchor (first input) is sacred: exact hex, exact L, always slot 1.
3. Luminance is the backbone: 7 levels spread in L so the system reads in greyscale.
4. Scaffolding first: poles are the horizon; only a neutral near-black/near-white input becomes a pole.
5. Any input resolves to the same skeleton: distil (>5) or expand (<5).
6. Pathological inputs have explicit exits, never a collapse.
7. One core, many uses.
8. Transparent: every decision is logged and shown.

## Files
- `public/engine.js`: the engine. Pure, deterministic, no dependencies. `ENGINE_VERSION`.
- `public/app.js`: UI. State lives in the URL hash (`#c=096ED3-F9A730&cat=A5&seq=A9&div=A11w&pv=0`), so every link reproduces a system.
- `public/palettes.js`: Boss's own palettes only (metric_trails, metric_trails_extended, raimun2_seq_1..39), from the R sysdata.
- `public/index.html`, `public/styles.css`: mobile-native layout. The page wears the system it shows (bg = light pole, text = dark pole).
- `test/engine.test.mjs`: `node web/test/engine.test.mjs`. 58 cases (own palettes + edge inputs). Asserts determinism, 5 distinct core colours, exact anchor, L gap > 0.05 measured on the output hex, and the old-R regressions.
- `test/make_fixture.mjs`: `node web/test/make_fixture.mjs` writes `tests/testthat/fixtures/engine_cases.json` (60 cases). The R tests assert the package returns exactly those hex codes. Regenerate after any engine change and bump `ENGINE_VERSION` in both engines (`LC_ENGINE_VERSION` in R).

## Cross-engine rules
- No Math.hypot / Math.cbrt: plain sqrt and pow(x, 1/3), same order of operations in R.
- Sums are left-to-right (`reduce` in JS, `Reduce` in R, never `sum()`/`mean()` in the engine).
- Rounding to 0-255 is round-half-up (`floor(x*255+0.5)` in R).
- Near-ties in distillation (evenly spaced ramps) go to the first candidate with tolerance `TIE = 1e-9`.
- Verified Nov 2026: 60 fixture cases (2040 expectations) and 400 random inputs, 0 mismatches.

## Engine decisions
- Poles are always neutral greys, including the defaults (greys at the L of #1D1C1B and #F4F3F3).
- Pole capture needs L in the pole zone (±0.08 from default poles) AND chroma < 0.04. Captured poles are neutral greys at the input's L.
- Anchor = first non-pole input. If everything is a pole, a synthetic mid-grey anchors.
- Distil: deterministic k-means (farthest-point init from the anchor), returns medoids (real input colours).
- Expand: new hues placed greedily as far as possible from existing hues; chroma = mean chroma of chromatic inputs (0.12 if none, hue seed 240).
- Hue pivot: if circular variance of hues < 0.05, satellites 2 and 4 (by L) rotate ±40°. User can turn it off (monochrome systems).
- L ladder: anchor L fixed; the 4 satellites are split below/above the anchor to maximise the minimum gap. If the anchor is within one step of a pole, that pole moves outward (up to 0 or 1).
- Gamut mapping keeps L and hue, reduces chroma only.
- Categorical A natural order, B/C contrast jumps, D chroma floor 0.15 and +15° hue; n > 5 interpolates in OKLab.
- Sequential: core colour to light pole (L × 0.97, 4% of the hue).
- Divergent: complement at the same L and chroma as the base (both arms weigh the same in greyscale), centre = white or black pole.

## Deploy
`wrangler.jsonc`: static-assets Worker `loscolores`, custom domain `loscolores.raimundos.cl`. Workers Builds on `main`, root directory `web`, deploy command `npx wrangler deploy`. Every push to main deploys.

## Open
- MATLAB, Stata and C# snippets were checked by eye only (no interpreter available). R, Python and JS were executed.
