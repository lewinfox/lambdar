#' Use lambdar with your project
#'
#' @section Description:
#' Call this function to create the necessary files lambdar needs to work. Specifically:
#'
#' * A `_lambdar.yml` config file
#' * A `lambdar/` directory to house a copy of the R runtime we will load into your container
#'
#' @export
init <- function() {

  # If this fails we want to restore the dir to its previous state
  LAMBDAR_DIR <- lam_dir_path()
  LAMBDAR_RUNTIME_PATH <- lam_runtime_path()
  LAMBDAR_CONFIG_PATH <- lam_config_path()

  quiet <- lam_is_quiet()

  tryCatch(
    {
      if (!dir.exists(LAMBDAR_DIR)) {
        dir.create(LAMBDAR_DIR)
        if (!quiet) {
          cli::cli_alert_success("Creating {.path {LAMBDAR_DIR}} directory")
        }
      }

      if (!file.exists(lam_runtime_path())) {
        file.copy(
          system.file("runtime", "lambdar_runtime.R", package = "lambdar", mustWork = TRUE),
          LAMBDAR_RUNTIME_PATH
        )
        if (!quiet) {
          cli::cli_alert_success("Writing {.path {LAMBDAR_RUNTIME_PATH}}")
        }
      }

      write_dockerignore()

      build_config()
    },
    error = function(e) {
      warning(e)
      clean()
    }
  )
}


#' Build a `_lambdar.yml` file
#'
#' Scans your project folder for `.R` files, identified those containing `@lambda` tags, and writes
#' a `_lambdar.yml` config file containing relevant information.
#'
#'
#' @return A [lambdar_config] object, invisibly.
#' @export
build_config <- function() {

  quiet <- lam_is_quiet()

  if (!using_lambdar()) {
    if (!quiet) {
      cli::cli_alert_info("Lambdar has not been initialiased yet, doing it now")
    }
    init()
  }

  extra_params <- lam_scan_project()

  # If a file already exists we want to keep some fields that we can't calculate automatically.
  # Specifically:
  #
  # * aws_account_id
  # * aws_region
  # * linux_packages
  #
  # The rest we are happy to overwrite, or add to if there are entries in the config that we don't
  # find automatically.
  if (file.exists(lam_config_path())) {
    current_config <- lambdar_config_from_file()

    not_missing <- function(field) {
      !is.null(field) && length(field) > 0 && nchar(field) > 0
    }

    # aws_account_id
    if (not_missing(current_config$aws_account_id)) {
      extra_params$aws_account_id <- current_config$aws_account_id
    }

    # aws_region
    if (not_missing(current_config$aws_region)) {
      extra_params$aws_region <- current_config$aws_region
    }

    # linux_packages
    if (not_missing(current_config$linux_packages)) {
      extra_params$linux_packages <- current_config$linux_packages
    }

    # r_packages
    if (not_missing(current_config$r_packages)) {
      extra_params$r_packages <- union(extra_params$r_packages, current_config$r_packages)
    }

    # lambda_handlers
    if (not_missing(current_config$lambda_handlers)) {
      extra_params$lambda_handlers <- union(extra_params$lambda_handlers, current_config$lambda_handlers)
    }

    # env
    if (not_missing(current_config$env)) {
      extra_params$env <- current_config$env
    }

    # Remove the old config file to prevent us being asked to confirm overwriting it.
    unlink(lam_config_path(), force = TRUE)
  }

  cfg <- new_lambdar_config(extra_params)

  write_config(cfg)

  invisible(cfg)
}

#' Create a Dockerfile
#'
#' Writes a Dockerfile to your project root directory based on a configuration file If no config
#' file path is provided it will try and read `_lambdar.yml` from your project's root directory.
#'
#' @export
#'
#' @return A [lambdar_config] object, invisibly. This is for internal use only and may be removed in
#'   future.
build_dockerfile <- function() {
  quiet <- lam_is_quiet()
  tryCatch(
    {
      if (!config_exists()) {
        build_config()
      }

      cfg <- lambdar_config_from_file()

      # Validate the config object
      if (!is_lambdar_config(cfg)) {

        if (!quiet) {
          msg <- "{.var cfg} must be a {.code lambdar_config} object. See {.code ?lambdar_config}."
          cli::cli_alert_danger(msg)
        }

        rlang::abort("Invalid configuration object")
      }

      # TODO: Better way of determining this
      if (length(cfg$lambda_handlers) > 1) {
        msg <- glue::glue(
          paste("Multiple handlers present, only the first (`{cfg$lambda_handlers[[1L]]}`)", "
                will be used.")
        )
        rlang::warn(msg)
      }

      # Build and write the Dockerfile
      write_dockerfile(cfg)

      if (!quiet) {
        msg <- paste("To build your container, run {.code docker build -t {cfg$app_name} .}",
                     "or {.fn lambdar::build_image}")
        cli::cli_alert_info(msg)
      }
      return(invisible(cfg))
    },
    lambdar_no_config = function(e) {
      return(invisible())
    }
  )
}


#' Build a container image from a config file
#'
#' Reads your `_lambdar.yml` config file, creates a Dockerfile from it, and then attempts to create
#' a container from the Dockerfile using `docker build`. You must have Docker installed for this to
#' work, otherwise it will throw an error.
#'
#' @export
build_image <- function() {

  quiet <- lam_is_quiet()

  # Check we have Docker installed
  if (!lam_has_docker()) {
    if (!quiet) {
      msg <- paste(
        "Can't build a container without Docker installed.",
        "You can still create a Dockerfile using {.fn lambdar::build_dockerfile}."
      )
      cli::cli_alert_danger(msg)
    }

    rlang::abort("Docker is not installed")
  }

  # No Dockerfile, no joy
  if (!dockerfile_exists()) {
    if (!quiet) {
      msg <- glue::glue("{lam_dockerfile_path()} does not exist - building")
      cli::cli_alert_info(msg)
    }
    # Suppress Dockerfile build messages.
    withr::with_options(list(lambdar.quiet = TRUE), {
      build_dockerfile()
    })
  }

  cfg <- lambdar_config_from_file()

  # Build the container
  docker_build_cmd <- glue::glue("docker build -t {cfg$app_name} .")
  exit_code <- lam_run_system_command(docker_build_cmd)

  # Check for success
  if (exit_code != 0) {
    rlang::abort("Docker build failure")
  }

  # Let the user know we are OK and provide useful URLs for testing
  if (!quiet) {
    cli::cli_alert_success("Docker build successful")
  }

  if (length(cfg$lambda_handlers) > 1 && !quiet) {
    msg <- "To start your container run {.code docker run -p 9000:8080 {cfg$app_name} <handler>}"
    cli::cli_alert_info(msg)
    cli::cli_alert_info("Possible values of {.code <handler>} are {.code {cfg$lambda_handlers}}")
  } else if (!quiet) {
    msg <- paste("To start your container run" ,
                 "{.code docker run -p 9000:8080 {cfg$app_name} {cfg$lambda_handlers}}")
    cli::cli_alert_info(msg)
  }
  if (!quiet) {
    msg <- paste("Once running you can send test queries to",
                 "{.code http://localhost:9000/2015-03-31/functions/function/invocations}")
    cli::cli_alert_info(msg)
  }

}

#' Remove all the lambdar-related files and directories from a project
#'
#' @export
clean <- function() {
  quiet <- lam_is_quiet()
  unlink(lam_dir_path(), recursive = TRUE, force = TRUE)
  unlink(lam_dockerfile_path(), force = TRUE)
  unlink(lam_dockerignore_path(), force = TRUE)
  unlink(lam_config_path(), force = TRUE)
  if (!quiet) {
    cli::cli_alert_success("Cleaned")
  }
}

#' Upload your container image to the Elastic Container Registry
#'
#' @export
upload_to_ecr <- function() {
  # TODO: Check container exists with the tag we expect
  cfg <- lambdar_config_from_file()
  # tag
  lam_ecr_tag_image_for_upload()
  lam_ecr_upload_image(cfg$aws_account_id, cfg$aws_region, cfg$app_name)
}
