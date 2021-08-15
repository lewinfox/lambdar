# `R CMD check` oes not run in a project environment so the functions that depend on
# `usethis::proj_path()` (i.e. most of them) break. This allows us to selectively skip tests we know
# will fail.
tests_running_in_project <- function() {
  !inherits(try(usethis::proj_path(), silent = TRUE), "try-error")
}
