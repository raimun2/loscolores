# loscolores

A perceptual color system generator (OKLab) for R. Give it a couple of
reference colors (say, your brand colors) and the engine derives a complete
system: 5 perceptually-separated base colors, neutral black/white poles, and
categorical, sequential, and divergent variants ready for `ggplot2` or base R
plots.

The same engine runs on the web at <https://loscolores.raimundos.cl>, where you
can see how the system is built and export it as a one-line object for R,
Python, MATLAB, Stata, JS or C#. Both engines return exactly the same colours.

## The rules

1. The first colour is the **anchor**: its exact value is kept, always in slot 1.
2. **Luminance is the backbone**: the 7 levels (2 poles + 5 core) are spread in
   lightness so the system reads in greyscale. Out-of-gamut colours lose chroma,
   never lightness.
3. **Poles are neutral greys.** A near-black or near-white input becomes a pole
   only if it is neutral; a navy or a pale yellow stays a colour.
4. Any input resolves to the same skeleton: more than 5 colours are distilled
   (real input colours are kept), fewer are expanded with hues placed as far
   apart as possible.
5. Every decision is logged: `print(get_loscolores())` shows how the system
   got there.

## Installation

```r
# install.packages("remotes")
remotes::install_github("raimun2/loscolores")
```

## Quick start

The package ships with the Metric Trails palette active by default. To use
your own:

```r
library(loscolores)

# From your own colors (HEX or R color names)
set_loscolores("#096ED3", "#F9A730", "#F99DC2")

# ...or from a palette already bundled with the package
set_loscolores("Set1")          # ColorBrewer
set_loscolores("Zissou1")       # wesanderson
set_loscolores("metric_trails") # the built-in default
```

`set_loscolores()` computes the whole system once and keeps it active for the
rest of the script — no need to pass it again in every plot.

When all the input hues are nearly the same, the engine rotates two satellites
±40° to separate them. For a deliberately monochrome system, turn that off:

```r
set_loscolores("#08306B", "#2171B5", "#6BAED6", pivot = FALSE)
```

To see the full system (input -> 5 base colors + poles -> categorical /
sequential / divergent variants) in a single panel:

```r
plot(get_loscolores())
```

## Using it with ggplot2

```r
library(ggplot2)

set_loscolores("#096ED3", "#F9A730", "#F99DC2", "#303F47")

ggplot(mpg, aes(class, fill = class)) +
  geom_bar() +
  scale_fill_loscolores_cat() +
  theme_loscolores()
```

**Categorical** — for discrete variables (`Option_A` through `Option_D`):

```r
ggplot(mpg, aes(class, fill = class)) +
  geom_bar() +
  scale_fill_loscolores_cat(option = "Option_B")
```

**Sequential** — for continuous variables (`Option_A` through `Option_E`,
one per core colour, each running to the light pole):

```r
ggplot(faithful, aes(waiting, eruptions, color = eruptions)) +
  geom_point(size = 3) +
  scale_color_loscolores_seq(option = "Option_A")
```

**Divergent** — for continuous variables with a meaningful midpoint (zero,
mean, etc.), centered on white or black. Each arm uses a core colour and its
complement at the same lightness, so both sides weigh the same:

```r
mtcars$mpg_c <- scale(mtcars$mpg)[, 1]

ggplot(mtcars, aes(factor(cyl), mpg_c, fill = mpg_c)) +
  geom_col() +
  scale_fill_loscolores_div(option = "Option_A", center = "white")
```

Every `scale_*` accepts `grayscale = TRUE` to preview how the palette holds up
in grayscale (print, colorblindness).

## Using it in base R plots

The `ggplot2` scales are thin wrappers around `sample_system()`, which you can
also call directly to get a plain color vector:

```r
sys <- get_loscolores()

# barplot()
pal_cat <- sample_system(sys, "categorical", "Option_A", n = 5)
barplot(table(mtcars$cyl), col = pal_cat, border = "white")

# native plot()
plot(mtcars$wt, mtcars$mpg,
     col = pal_cat[factor(mtcars$cyl)],
     pch = 19, cex = 1.5)

# image() -- needs a continuous gradient
pal_seq <- sample_system(sys, "sequential", "Option_A", n = 100)
z <- outer(1:20, 1:20, function(x, y) x + y)
image(z, col = pal_seq)

# heatmap() with a divergent palette
pal_div <- sample_system(sys, "divergent", "Option_A", n = 101, center = "white")
heatmap(cor(mtcars), col = pal_div, symm = TRUE)
```

`sample_system(sys, type, option, n, center, grayscale)`:

| Argument    | Values                                       |
|-------------|-----------------------------------------------|
| `type`      | `"categorical"`, `"sequential"`, `"divergent"` |
| `option`    | `"Option_A"`-`"Option_D"` (cat.) / `"Option_A"`-`"Option_E"` (seq./div.) |
| `n`         | number of colors to return                    |
| `center`    | `"white"` or `"black"` (`divergent` only)      |
| `grayscale` | `TRUE`/`FALSE` -- desaturate the output        |

## Diagnostics

```r
system_report(get_loscolores())   # luminance ladder, minimum gap, chroma, hue variance
print(get_loscolores())           # swatches plus the log of every decision
```

## Built-in known palettes

Beyond your own combinations, the package bundles an internal library of
named palettes (ColorBrewer, wesanderson, viridis, ggsci, ggthemes, Metric
Trails, and others), accessible by name:

```r
set_loscolores("Dark2")
set_loscolores("Hiroshige")
```
