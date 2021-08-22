.onLoad <- function(libname, pkgname) {
  options(
    lambdar.quiet = FALSE
  )
}

.onUnload <- function(libpath) {
  options(
    lambdar.quiet = NULL
  )
}
