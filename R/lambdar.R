#' Use lambdar with your project
#'
#' @section Description:
#' Call this function to create the necessary files lambdar needs to work. Specifically:
#'
#' * A `_lambdar.yml` config file
#' * A `lambdar/` directory to house a copy of the R runtime we will load into your container
#'
#' The function will try and pre-populate your config file with as much information as possible, but
#' you are free to amend as much or as little as you like.
#'
#' @export
use_lambdar <- function() {
  # If this fails we want to restore the dir to its previous state
  LAMBDAR_DIR <- lam_dir_path()
  LAMBDAR_RUNTIME_PATH <- lam_runtime_path()
  LAMBDAR_CONFIG_PATH <- lam_config_path()

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
      build_config()
      conf_str <- usethis::ui_path(LAMBDAR_CONFIG_PATH)
      usethis::ui_todo("Edit {conf_str}")
    },
    error = function(e) {
      if (dir.exists(LAMBDAR_DIR)) {
        unlink(LAMBDAR_DIR, recursive = TRUE, force = TRUE)
        cli::cli_alert_warning("Removing {.path {LAMBDAR_DIR}} directory")
      }
      if (file.exists(LAMBDAR_CONFIG_PATH)) {
        unlink(LAMBDAR_CONFIG_PATH, "_lambdar.yml", force = TRUE)
        cli::cli_alert_warning("Removing {.path {LAMBDAR_CONFIG_PATH}}")
      }
      stop(e)
    }
  )
}


#' Build a `_lambdar.yml` file
#'
#' Scans your project folder for `.R` files, identified those containing `@lambda` tags, and writes
#' a `_lambdar.yml` config file containing relevant information.
#'
#' @export
build_config <- function() {
  if (!using_lambdar()) {
    cli::cli_alert_info("Lambdar has not been initialiased yet, doing it now")
    use_lambdar()
  }

  # TODO: Look for source() calls and follow that graph.

  # These are parameters that are not set in the default config. They also need to be formatted
  # before we pass them to `usethis::use_template()`
  include_files <- lam_handler_filenames()
  r_packages <- lam_get_file_dependencies(include_files)

  extra_params <- list(
    lambda_handlers = lam_build_quoted_list(lam_parse_project_handlers()),
    include_files = lam_build_quoted_list(include_files),
    r_packages = r_packages
  )

  cfg <- new_lambdar_config(extra_params)

  usethis::use_template(
    "_lambdar.yml",
    save_as = "_lambdar.yml",
    data = cfg,
    package = "lambdar"
  )
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
      build_config()

      # If this is called in standalone mode, read the config file. Otherwise we expect to have a
      # `cfg` object passed in
      if (is.null(cfg)) {
        cfg <- lambdar_config_from_file(lam_config_path())
      }

      # Validate the config object
      if (!is_lambdar_config(cfg)) {
        msg <- "{.var cfg} must be a {.code lambdar_config} object. See {.code ?lambdar_config}."
        cli::cli_alert_danger(msg)
        rlang::abort("Invalid configuration object")
      }

      # Some elements of the config file need formatting before they're inserted into the Dockerfile
      # template
      cfg$r_packages      <- lam_build_quoted_list(cfg$r_packages)
      cfg$r_package_repos <- lam_build_quoted_list(cfg$r_package_repos)
      cfg$linux_packages  <- lam_build_space_separated_list(cfg$linux_packages)
      cfg$include_files   <- lam_build_quoted_list(cfg$include_files)
      cfg$env             <- lam_build_env_list(cfg$env)
      cfg$r_runtime_file  <- lam_runtime_path()

      # Replace any zero-length elements with NULL to prevent the populating the Dockerfile
      cfg <- lapply(cfg, function(item) if (length(item) > 0) item else NULL)

      # Build and write the Dockerfile
      usethis::use_template("Dockerfile", save_as = "Dockerfile", data = cfg, package = "lambdar")
      if (!quiet) {
        msg <- paste("To build your container, run {.code docker build -t {cfg$app_name} .}",
                     "or {.code lambdar::build_container()}")
        cli::cli_alert_info(msg)
      }
    },
    lambdar_no_config = function(e) {
      return(invisible())
    }
  )
}


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

  # Read config and create the Dockerfile
  cfg <- lambdar_config_from_file(lam_config_path())
  build_dockerfile(cfg, quiet = TRUE)

  # Build the container
  docker_build_cmd <- glue::glue("docker build -t {cfg$app_name} .")
  exit_code <- system(docker_build_cmd)

  # Check for success
  if (exit_code != 0) {
    rlang::abort("Docker build failure")
  }

  # Let the user know we are OK and provide useful URLs for testing
  cli::cli_alert_success("Docker build successful")
  if (length(cfg$lambda_handlers) > 1) {
    msg <- "To start your container run {.code docker run -p 9000:8080 {cfg$app_name} <handler>}"
    cli::cli_alert_info(msg)
    cli::cli_alert_info("Possible values of {.code <handler>} are {.code {cfg$lambda_handlers}}")
  } else {
    msg <- paste("To start your container run" ,
                 "{.code docker run -p 9000:8080 {cfg$app_name} {cfg$lambda_handlers}}")
    cli::cli_alert_info(msg)
  }
  msg <- paste("Once running you can send test queries to",
               "{.code http://localhost:9000/2015-03-31/functions/function/invocations}")
  cli::cli_alert_info(msg)
}
