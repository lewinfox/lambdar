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
  usethis::proj_get()
  if (file.exists(lam_dockerfile_path())) {
    cli::cli_alert_warning("{.path Dockerfile} already exists")
  } else {
    cli::cli_alert_info("Building {.path Dockerfile}")
  }
  build_dockerfile()
  docker_build_cmd <- glue::glue("docker build -t lewinfox/lambdar .")
  system(docker_build_cmd)
}

#' Create a Dockerfile
#'
#' @return Nothing is returned, but a Dockerfile will be written to disk.
#'
#' @export
build_dockerfile <- function() {
  tryCatch(
    {
      c <- lam_read_config()
      lam_build_dockerfile(
        r_functions_file = c$r_functions_file,
        r_packages = c$r_packages,
        repos = c$repos,
        linux_packages = c$linux_packages,
        r_version = c$r_version,
        include_files = c$include_files,
        env = c$env
      )
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
  # Remove any empty lists to prevent them getting included in the Dockerfile
  config_list <- lapply(yaml::read_yaml(cfg), function(item) if (length(item) == 0) NULL else item)
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
#' @param env Named list containing environment variables to be set in the container.
#'
#' @return Nothing - this function is called for its side effect, which is to write a `Dockerfile`
#'   to disk.
#' @keywords internal
lam_build_dockerfile <- function(r_functions_file = "main.R",
                                 r_packages = NULL,
                                 repos = getOption("repos"),
                                 linux_packages = NULL,
                                 r_version = lam_r_version(),
                                 include_files = NULL,
                                 env = list()) {
  # Clean up the file paths
  r_functions_file <- relish(file.path(usethis::proj_get(), r_functions_file))
  r_runtime_file <- relish(file.path(usethis::proj_get(), "lambdar", "lambdar_runtime.R"))

  # Create the data list
  data = list(
    r_version = r_version,
    r_packages = lam_build_quoted_list(r_packages),
    r_package_repos = lam_build_quoted_list(repos),
    r_runtime_file = r_runtime_file,
    r_functions_file = r_functions_file,
    linux_packages = lam_build_space_separated_list(linux_packages),
    include_files = lam_build_space_separated_list(include_files),
    env = lam_build_env_list(env)
  )

  # Build and write the Dockerfile
  usethis::use_template("Dockerfile", save_as = "Dockerfile", data = data, package = "lambdar")
}

#' Add `_lambdar.yml` to the project root.
#'
#' @keywords internal
use_lambdar_yaml <- function() {
  # TODO: Identify and pre-populate the main file (if it already contains the `@lambdar` tag this
  #       should be easy), and populate the `include_files` option with a list of everything else in
  #       the directory that isn't lambdar-related
  data <- list(
    r_version = lam_r_version()
  )
  usethis::use_template("_lambdar.yml", save_as = "_lambdar.yml", data = data, package = "lambdar")
  usethis::ui_todo(paste("Edit", usethis::ui_path("_lambdar.yml")))
}

#' Create a space-separated list of environment variables
#'
#' @param env A named list
lam_build_env_list <- function(env = list()) {
  if (!is.list(env)) {
    rlang::abort("`env` must be a list, not {typeof(env)})")
  }
  if (length(env) == 0) {
    return(NULL)
  }
  if (is.null(names(env)) || any(names(env) == "")) {
    rlang::abort("All elements of `env` must be named")
  }
  vars = sapply(names(env), function(name) paste0(toupper(name), "=\"", env[[name]], "\""))
  lam_build_space_separated_list(vars)
}

#' Use lambdar with your project
#'
#' @return Nothing
#' @export
use_lambdar <- function() {
  # If this fails we want to restore the dir to its previous state
  current_files <- list.files(usethis::proj_get(), all.files = TRUE, full.names = TRUE)
  LAMBDAR_DIR <- file.path(usethis::proj_get(), "lambdar")
  LAMBDAR_RUNTIME_PATH <- file.path(LAMBDAR_DIR, "lambdar_runtime.R")

  tryCatch(
    {
      if (!dir.exists(LAMBDAR_DIR)) {
        dir.create(LAMBDAR_DIR)
        cli::cli_alert_success("Creating {.path {LAMBDAR_DIR}} directory")
      }
      file.copy(
        system.file("runtime", "lambdar_runtime.R", package = "lambdar", mustWork = TRUE),
        LAMBDAR_RUNTIME_PATH
      )
      cli::cli_alert_success("Writing {.path {LAMBDAR_RUNTIME_PATH}}")
      use_lambdar_yaml()
    },
    error = function(e) {
      cli::cli_alert_danger("{e}")
      unlink(LAMBDAR_DIR, recursive = TRUE, force = TRUE)
      cli::cli_alert_warning("Removing {.PATH {LAMBDAR_DIR}}")
      unlink(usethis::proj_get(), "_lambdar.yml", force = TRUE)
      cli::cli_alert_warning("Removing {.path _lambdar.yml}")
    }
  )
}
