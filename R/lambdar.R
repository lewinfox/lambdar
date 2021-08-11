#' Build a container image from a config file
#'
#' Reads your `_lambdar.yml` config file, creates a Dockerfile from it, and then attempts to create
#' a container from the Dockerfile using `docker build`. You must have Docker installed for this
#' to work, otherwise it will throw an error.
#'
#' @export
build_container <- function() {

  # Check we have Docker installed
  if (!lam_has_docker()) {
    msg <- paste(
      "Can't build a container without Docker installed.",
      "You can still create a Dockerfile using {.fn lambdar::build_dockerfile}."
    )
    cli::cli_alert_danger(msg)
    return(invisible())
  }

  # Warn if we are overwriting an existing Dockerfile (usethis::use_template() will also warn)
  if (file.exists(lam_dockerfile_path())) {
    cli::cli_alert_warning("{.path Dockerfile} already exists")
  } else {
    cli::cli_alert_info("Building {.path Dockerfile}")
  }

  # Read config and create the Dockerfile
  cfg <- lambdar_config(lam_config_file())
  build_dockerfile(cfg, quiet = TRUE)

  # Build the container
  docker_build_cmd <- glue::glue("docker build -t {cfg$name} .")
  exit_code <- system(docker_build_cmd)

  # Check for success
  if (exit_code != 0) {
    cli::cli_alert_danger(paste("Docker build failed with exit code", exit_code))
    rlang::abort("Docker build failure")
  }

  # Let the user know we are OK and provide useful URLs for testing
  cli::cli_alert_success("Docker build successful")
  cli::cli_alert_info("To start your container run {.code docker run -p 9000:8080 {cfg$name} {cfg$lambda_handler}}")
  cli::cli_alert_info("API endpoint: {.code http://localhost:9000/2015-03-31/functions/function/invocations}")
}

#' Create a Dockerfile
#'
#' Writes a Dockerfile to your project root directory based on a configuration object. If no
#' config object is provided it will try and read `_lambdar.yml` from your project's root directory.
#'
#' @param cfg A [lambdar_config()] object (optional).
#' @param quiet If `FALSE`, will tell you the command to build your container.
#'
#' @export
build_dockerfile <- function(cfg = NULL, quiet = FALSE) {
  tryCatch(
    {
      # If this is called in standalone mode, read the config file. Otherwise we expect to have a
      # `cfg` object passed in
      if (is.null(cfg)) {
        cfg <- lambdar_config(lam_config_file())
      }

      # Validate the config object
      if (!is_lambdar_config(cfg)) {
        cli::cli_alert_danger("{.var cfg} must be a {.code lambdar_config} object created by {.fn lambdar_config}")
        rlang::abort("Invalid configuration object")
      }

      # Some elements of the config file need formatting before they're inserted into the Dockerfile
      # template
      cfg$r_packages      <- lam_build_quoted_list(cfg$r_packages)
      cfg$r_package_repos <- lam_build_quoted_list(cfg$r_package_repos)
      cfg$linux_packages  <- lam_build_space_separated_list(cfg$linux_packages)
      cfg$include_files   <- lam_build_space_separated_list(cfg$include_files)
      cfg$env             <- lam_build_env_list(cfg$env)
      cfg$r_runtime_file  <- lam_runtime_path()

      # If repos are provided, use them. If not, use `getOption("repos")`
      if (is.null(cfg$r_package_repos)) {
        cfg$r_package_repos <- getOption("repos")
      }
      cfg$r_package_repos <- lam_build_quoted_list(cfg$r_package_repos)

      # Replace any zero-length elements with NULL to prevent the populating the Dockerfile
      cfg <- lapply(cfg, function(item) if (length(item) > 0) item else NULL)

      # Build and write the Dockerfile
      usethis::use_template("Dockerfile", save_as = "Dockerfile", data = cfg, package = "lambdar")
      if (!quiet) {
        cli::cli_alert_info("To build your container, run {.code docker build -t {cfg$name} .}")
      }
    },
    lambdar_no_config = function(e) {
      return(invisible())
    }
  )
}

#' Use lambdar with your project
#'
#' @section Description:
#' Call this function to create the necessary files lambdar needs to work. Specifically:
#'
#' * A `_lamndar.yml` config file
#' * A `lambdar/` directory to house a copy of the R runtime we will load into your container.
#'
#' The function will try and pre-populate your config file with as much information as possible, but
#' you are free to amend as much or as little as you like.
#'
#' @export
use_lambdar <- function() {
  # If this fails we want to restore the dir to its previous state
  LAMBDAR_DIR <- lam_dir_path()
  LAMBDAR_RUNTIME_PATH <- lam_runtime_path()

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
      # TODO: Identify and pre-populate the main file (if it already contains the `@lambdar` tag this
      #       should be easy), and populate the `include_files` option with a list of everything else in
      #       the directory that isn't lambdar-related
      data <- list(
        user = Sys.getenv("USER"),
        r_version = lam_r_version(),
        r_package_repos = getOption("repos"),
        proj_root = basename(usethis::proj_get()),
        lambda_handlers = lam_build_quoted_list(lam_parse_project_handlers())
      )
      usethis::use_template("_lambdar.yml", save_as = "_lambdar.yml", data = data, package = "lambdar", open = TRUE)
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
