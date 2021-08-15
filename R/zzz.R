.onLoad <- function(libname, pkgname) {
  # This is to get the printing of "setting active project to ..." out of the way as soon as
  # possible
  usethis::proj_get()
}
