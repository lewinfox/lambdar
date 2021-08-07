#' Build a Dockerfile
#'
#' @param r_functions_file File containing the R functions to be lambda-d
#' @param packages Character vector of R packages that need to be installed in the container
#' @param repos R package repo/s to use. Optional, defaults to the value of your local
#'   `getOption("repos")`.
#' @param r_version String - R version to be installed in the lambda container, e.g. "4.0.1".
#'   Defaults to your local R version.
#'
#' @return Nothing - this function is called for its side effect, which is to write a `Dockerfile`
#'   to disk.
#' @keywords internal
lam_build_dockerfile <- function(r_functions_file = "main.R", packages = NULL, repos = getOption("repos"), r_version = lam_r_version()) {
  outfile <- "Dockerfile"
  data = list(
    r_version = r_version,
    r_packages = lam_build_quoted_list(packages),
    r_package_repos = lam_build_quoted_list(repos),
    r_runtime_file = relish(system.file("runtime.R", package = "lambdar", mustWork = TRUE)),
    r_functions_file = relish(r_functions_file)
  )
  usethis::use_template("Dockerfile_template", save_as = outfile, data = data, package = "lambdar")
}

#' Convert a vector into a single-quoted list
#'
#' @param items Vector or items to write.
#'
#' @return A string
#' @keywords internal
#'
#' @examples
#' lam_build_quoted_list(c("a", "b", "c"))
lam_build_quoted_list <- function(items = NULL) {
  glue::glue_collapse(glue::single_quote(items), sep = ",")
}

#' Get the current R version
#'
#' @return Current R version as a string, e.g. "4.0.1"
#' @keywords internal
lam_r_version <- function() {
  paste0(R.version$major, ".", R.version$minor)
}
