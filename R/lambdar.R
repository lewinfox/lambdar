#' Build a container image from a config file
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
#' @param cfg A [lambdar_config] object.
#' @param quiet If `FALSE`, will tell you the command to build your container.
#'
#' @return Nothing is returned, but a Dockerfile will be written to disk.
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
  config_list[["r_functions_file"]] <- strsplit(config_list[["lambda_function"]], split = ".", fixed = TRUE)[[1]][[1]]
  config_list
}

#' Add `_lambdar.yml` to the project root.
#'
#' @keywords internal
use_lambdar_yaml <- function() {
  # TODO: Identify and pre-populate the main file (if it already contains the `@lambdar` tag this
  #       should be easy), and populate the `include_files` option with a list of everything else in
  #       the directory that isn't lambdar-related
  data <- list(
    user = Sys.getenv("USER"),
    r_version = lam_r_version(),
    r_package_repos = getOption("repos"),
    proj_root = basename(usethis::proj_get()),
    lambda_handler = "main.hello_world"
  )
  usethis::use_template("_lambdar.yml", save_as = "_lambdar.yml", data = data, package = "lambdar", open = TRUE)
}

#' Create a space-separated list of environment variables
#'
#' @param env A named list
lam_build_env_list <- function(env = list()) {
  if (!is.list(env)) {
    msg <- glue::glue("`env` must be a list, not {typeof(env)})")
    rlang::abort(msg)
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
