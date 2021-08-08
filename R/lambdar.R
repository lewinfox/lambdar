#' Build a Dockerfile
#'
#' @param r_functions_file File containing the R functions to be lambda-d
#' @param packages Character vector of R packages that need to be installed in the container
#' @param repos R package repo/s to use. Optional, defaults to the value of your local
#'   `getOption("repos")`.
#' @param linux_packages Character vector of Linux packages to be installed in the container.
#' @param r_version String - R version to be installed in the lambda container, e.g. "4.0.1".
#'   Defaults to your local R version.
#'
#' @return Nothing - this function is called for its side effect, which is to write a `Dockerfile`
#'   to disk.
#' @keywords internal
lam_build_dockerfile <- function(r_functions_file = "main.R",
                                 r_packages = NULL,
                                 repos = getOption("repos"),
                                 linux_packages = NULL,
                                 r_version = lam_r_version(),
                                 include_files = NULL) {
  outfile <- file.path(usethis::proj_get(), "Dockerfile")
  data = list(
    r_version = r_version,
    r_packages = lam_build_quoted_list(r_packages),
    r_package_repos = lam_build_quoted_list(repos),
    r_runtime_file = relish(system.file("runtime.R", package = "lambdar", mustWork = TRUE)),
    r_functions_file = relish(r_functions_file),
    linux_packages = lam_build_space_separated_list(linux_packages),
    include_files = lam_build_space_separated_list(include_files)
  )
  usethis::use_template("Dockerfile", save_as = outfile, data = data, package = "lambdar")
}

#' Add `_lamdar.yml` to the project root.
#'
#' @keywords internal
use_lambdar_yaml <- function(save_as = "_lambdar.yml") {
  # TODO: Identify and pre-populate the main file (if it already contains the `@lamdar` tag this
  #       should be easy), and populate the `include_files` option with a list of everything else in
  #       the directory that isn't lamdar-related
  data <- list(
    r_version = lam_r_version()
  )
  usethis::use_template("_lambdar.yml", save_as = save_as, data = data, package = "lambdar")
}

#' Use lamndar with your project
#'
#' @return Nothing
#' @export
use_lambdar <- function() {
  cli::cli_alert_success("Setting up a lambdar project")
  use_lambdar_yaml(file.path(usethis::proj_get(), "_lambdar.yml"))
}
