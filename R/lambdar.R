#' Build a container image from a config file
#'
#' @export
build_container <- function() {
  if (!lam_has_docker()) {
    msg <- paste(
      "Can't build a container without Docker installed.",
      "You can still create a Dockerfile using {.fn lambdar::build_dockerfile}."
    )
    cli::cli_alert_danger(msg)
    return(invisible())
  }
}

#' Create a Dockerfile
#'
#' @return Nothing is returned, but a Dockerfile will be written to disk.
#'
#' @export
build_dockerfile <- function() {
  tryCatch(
    {
      lam_build_dockerfile(lam_read_config())
    },
    lambdar_no_config = function(e) {
      return(invisible())
    }
  )
}

#' Read and validate `_lambdar.yml`
#'
#' @return A list containing parsed config data.
#'
#' @keywords internal
lam_read_config <- function() {
  cfg <- lam_config_file()
  if (!file.exists(cfg)) {
    cli::cli_alert_danger("Config file {.path {cfg}} not found. Have you run {.fn lambdar::use_lambdar}?")
    rlang::abort("No config file found", "lambdar_no_config")
  }
  config_list <- yaml::read_yaml(cfg)
  # TODO: Validate config
  config_list
}

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
  outfile <- lam_dockerfile_path()
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

#' Add `_lambdar.yml` to the project root.
#'
#' @keywords internal
use_lambdar_yaml <- function(save_as = "_lambdar.yml") {
  # TODO: Identify and pre-populate the main file (if it already contains the `@lambdar` tag this
  #       should be easy), and populate the `include_files` option with a list of everything else in
  #       the directory that isn't lambdar-related
  data <- list(
    r_version = lam_r_version()
  )
  usethis::use_template("_lambdar.yml", save_as = save_as, data = data, package = "lambdar")
  usethis::ui_todo(paste("Edit", save_as))
}

#' Use lambdar with your project
#'
#' @return Nothing
#' @export
use_lambdar <- function() {
  cli::cli_alert_success("Setting up a lambdar project")
  use_lambdar_yaml(file.path(usethis::proj_get(), "_lambdar.yml"))
}
