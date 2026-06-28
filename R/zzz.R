# ==============================================================================
# zzz.R
# Package bootstrapping: initialize default system on load
# ==============================================================================

.onLoad <- function(libname, pkgname) {
  tryCatch({
    # `paletas` is available in the package namespace via sysdata.rda
    seed_hex    <- paletas[[1L]]$hexcolors
    sys_default <- loscolores_system(seed_hex)
    options(loscolores.default_system = sys_default)
  }, error = function(e) {
    warning(
      sprintf(
        "loscolores: no se pudo inicializar el sistema por defecto: %s\n",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "loscolores cargado. Usa set.loscolores() para activar tu paleta."
  )
}