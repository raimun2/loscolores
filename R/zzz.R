# ==============================================================================
# zzz.R
# Package bootstrapping: initialize the Metric Trails default system on load
# ==============================================================================

# These names are data-frame columns referenced inside ggplot2::aes()
# (methods_s3.R) via non-standard
# evaluation. R CMD check can't tell they're columns, not undefined globals -
# this declaration silences that NOTE without changing any behavior.
utils::globalVariables(c(
  "pos", "level", "hex", "wid", "variant", "y", "x_pos"    # methods_s3.R
))

# The default system on load is sourced from internal sysdata - the
# "metric_trails" entry of `paletas` - instead of a hardcoded literal vector,
# so there's a single source of truth for the brand anchors.
.onLoad <- function(libname, pkgname) {
  tryCatch({
    seed_hex    <- paletas[["metric_trails"]]$hexcolors
    sys_default <- loscolores_system(seed_hex)
    options(loscolores.default_system = sys_default)
  }, error = function(e) {
    warning(
      sprintf(
        "loscolores: could not initialize the default system: %s\n",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "loscolores loaded with the Metric Trails palette as default. ",
    "Use set_loscolores() to activate your own palette."
  )
}
