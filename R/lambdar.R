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

      usethis::use_template(
        "_lambdar.yml",
        save_as = "_lambdar.yml",
        package = "lambdar"
      )

    },
    error = function(e) {
      warning(e)
      clean(dir)
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

  quiet <- lam_is_quiet()

  if (!using_lambdar()) {
    if (!quiet) {
      cli::cli_alert_info("Lambdar has not been initialiased yet, doing it now")
    }
    init()
  }

  # TODO: Look for source() calls and follow that graph.

  # These are parameters that are not set in the default config. They also need to be formatted
  # before we pass them to `usethis::use_template()`
  include_files <- lam_handler_filenames(dir)
  r_packages <- lam_get_file_dependencies(include_files)

  extra_params <- list(
    lambda_handlers = lam_build_quoted_list(lam_parse_project_handlers(dir)),
    include_files = lam_build_quoted_list(include_files),
    r_packages = r_packages
  )

  # If a file already exists we want to keep some fields that we can't calculate automatically.
  # Specifically:
  #
  # * aws_account_id
  # * aws_region
  # * linux_packages
  #
  # The rest we are happy to overwrite
  if (file.exists(lam_config_path(dir))) {
    current_config <- lambdar_config_from_file(lam_config_path(dir))

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
  }

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
#' Writes a Dockerfile to your project root directory based on a configuration file If no config
#' file path is provided it will try and read `_lambdar.yml` from your project's root directory.
#'
#' @param from_scratch Boolean. If `TRUE`, lambdar will re-scan your project and rebuild your config
#'   file before building the Dockerfile. If `FALSE`, lambdar will use whatever state your config
#'   file is in and build a Dockerfile from that.
#' @param quiet If `FALSE`, will tell you the command to build your container.
#' @param lambda_handler Either `NULL` or a string specifying a handler function in
#'   `"file.function_name"` format. Only necessary if your project contains multiple handlers. In
#'   this case if no handler is specified the first one in the list will be used, with a warning.
#'
#' @export
#'
#' @return A [lambdar_config] object, invisibly. This is for internal use only and may be removed in
#'   future.
build_dockerfile <- function(from_scratch = TRUE, lambda_handler = NULL) {
  quiet <- lam_is_quiet()
  tryCatch(
    {
      if (from_scratch || !config_exists()) {
        build_config()
      }

      cfg <- lambdar_config_from_file(lam_config_path())

      # Validate the config object
      if (!is_lambdar_config(cfg)) {

        if (!quiet) {
          msg <- "{.var cfg} must be a {.code lambdar_config} object. See {.code ?lambdar_config}."
          cli::cli_alert_danger(msg)
        }

        rlang::abort("Invalid configuration object")
      }

      # TODO: Better way of determining this
      if (length(cfg$lambda_handlers) > 1 && is.null(lambda_handler)) {
        if (!quiet) {
          msg <- paste(
            "Multiple lambdar handlers found in config file and no handler given in the",
            "{.var lambda_handler} argument.\nThis is fine for testing, but when you come to build",
            "the final container you will need to specify your chosen handler when calling",
            "{.fn lambdar::build_image}.\nIn the meantime the Dockerfile has been written with",
            "{.code {cfg$lambda_handlers[[1L]]}} as the handler."
          )
          cli::cli_alert_warning(msg)
        }
        rlang::warn("Multiple handlers present, only the first will be used.")
      }

      lambda_entrypoint <- cfg$lambda_handlers[[1L]]

      # Some elements of the config file need formatting before they're inserted into the Dockerfile
      # template
      cfg$r_packages        <- lam_build_quoted_list(cfg$r_packages)
      cfg$r_package_repos   <- lam_build_quoted_list(cfg$r_package_repos)
      cfg$linux_packages    <- lam_build_space_separated_list(cfg$linux_packages)
      cfg$include_files     <- lam_build_quoted_list(cfg$include_files)
      cfg$env               <- lam_build_env_list(cfg$env)
      cfg$r_runtime_file    <- lam_runtime_path()
      cfg$lambda_entrypoint <- lam_build_quoted_list(lambda_entrypoint)

      # Replace any zero-length elements with NULL to prevent them populating the Dockerfile - if
      # this happens then `whisker` treats them as if they're present and may include bits of the
      # template that we don't want.
      cfg <- lapply(cfg, function(item) if (length(item) > 0) item else NULL)

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
#' @param from_scratch Boolean. If `TRUE`, lambdar will re-scan your project and rebuild your config
#'   file before building the container. If FALSE it will build from whatever the current state of
#'   your Dockerfile is.
#' @param lambda_handler Either `NULL` or a string specifying a handler function in
#'   `"file.function_name"` format.
#' @export
build_image <- function(from_scratch = TRUE, lambda_handler = NULL) {

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

  if (from_scratch) {
    # Read config and create the Dockerfile
    cfg <- build_dockerfile(from_scratch = from_scratch, quiet = TRUE, lambda_handler = lambda_handler)
  } else {
    cfg <- lambdar_config_from_file(lam_config_path())
  }

  # No Dockerfile, no joy
  if (!dockerfile_exists()) {
    msg <- glue::glue("{lam_dockerfile_path()} does not exist")
    rlang::abort(msg)
  }

  # Build the container
  docker_build_cmd <- glue::glue("docker build -t {cfg$app_name} .")
  exit_code <- system(docker_build_cmd)

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
  unlink(lam_config_path(), force = TRUE)
  if (!quiet) {
    cli::cli_alert_success("Cleaned")
  }
}

#' Upload your container image to the Elastic Container Repository
#'
#' @export
upload_to_ecr <- function() {
  # TODO: Check container exists with the tag we expect
  cfg <- lambdar_config_from_file()
  # tag
  lam_ecr_tag_image_for_upload()
  lam_ecr_upload_image(cfg$aws_account_id, cfg$aws_region, cfg$app_name)
}

#' Attempt to retrieve your AWS account id
#'
#' @return String
#'
#' @export
get_aws_account_id <- function() {
  lam_aws_get_account_id()
}
