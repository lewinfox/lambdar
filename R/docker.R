# ---- Docker ----

#' Do we have docker installed?
#'
#' Examines the output of [Sys.which()] to see if we can locate an installation.
#'
#' @return Boolean
#'
#' @keywords internal
lam_has_docker <- function() {
  if (Sys.which("docker") == "") {
    msg <- paste(
      "No docker installation found.",
      "Refer to {.url https://docs.docker.com/get-docker/} for installation instructions"
    )
    cli::cli_alert_warning(msg)
    return(FALSE)
  }
  TRUE
}

# ---- Dockerfile ----

#' Take a config object, format its contents and write the Dockerfile
#'
#' Formatting involves correctly quoting R vectors for inclusion in the config file.
#'
#' @param cfg A [lambdar_config] object.
#'
#' @keywords internal
write_dockerfile <- function(cfg) {
  if (!is_lambdar_config(cfg)) {
    msg <- glue::glue("`cfg` must be a lambdar_config object, not {class(cfg)}")
    rlang::abort(msg)
  }

  lambda_entrypoint <- cfg$lambda_handlers[[1L]]

  # Formatting
  cfg$r_packages        <- lam_build_quoted_list(cfg$r_packages)
  cfg$r_package_repos   <- lam_build_quoted_list(cfg$r_package_repos)
  cfg$linux_packages    <- lam_build_separated_list(cfg$linux_packages)
  cfg$env               <- lam_build_env_list(cfg$env)
  cfg$r_runtime_file    <- lam_runtime_path()
  cfg$lambda_entrypoint <- lam_build_quoted_list(lambda_entrypoint)

  cfg <- lapply(cfg, function(item) if (length(item) > 0) item else NULL)

  usethis::use_template("Dockerfile", data = cfg, package = "lambdar")
}

# ---- Dockerfile formatting helpers ----

#' Convert a vector into text
#'
#' Writing YAML and Dockerfiles templates requires translating R vectors into strings. In some cases
#' these need to be quoted (e.g. to build R [install.packages()] commands) and in other cases they
#' need to be space-separated (e.g. Linux `yum install a b c`).
#'
#' @param items Vector or items to write.
#'
#' @return A string
#'
#' @name build-lists
#' @keywords internal
NULL

#' @describeIn build-lists Build a double-quoted comma-separated list
#' @param quote `"single"` or `"double"`
lam_build_quoted_list <- function(items = NULL, quote = c("double", "single")) {
  # The templating function needs to receive NULLs if the item is not present otherwise sections
  # that are not supposed to render will render.
  if (is.null(items)) {
    return(NULL)
  }
  # We want to ensure they only end up with one quote if they're already quoted
  items <- stringr::str_remove_all(items, "[\"\']")
  quote <- match.arg(quote)
  quoting_function <- if (quote == "single") glue::single_quote else glue::double_quote
  as.character(glue::glue_collapse(quoting_function(items), sep = ", "))
}

#' @describeIn build-lists Build an unquoted space-separated list
lam_build_separated_list <- function(items = NULL, sep = " ") {
  # The templating function needs to receive NULLs if the item is not present otherwise sections
  # that are not supposed to render will render.
  if (is.null(items)) {
    return(NULL)
  }
  items <- stringr::str_remove_all(items, "[\"\']")
  as.character(glue::glue_collapse(items, sep = sep))
}

#' Get the current R version
#'
#' @return Current R version as a string, e.g. "4.0.1"
#' @keywords internal
lam_r_version <- function() {
  paste0(R.version$major, ".", R.version$minor)
}

#' Create a space-separated list of environment variables
#'
#' @param env A named list
#'
#' @keywords internal
lam_build_env_list <- function(env = NULL) {
  if (is.null(env)) {
    env <- list()
  }
  if (!is.list(env)) {
    msg <- glue::glue("`env` must be a list, not {typeof(env)})")
    rlang::abort(msg)
  }
  # The templating function needs to receive NULLs if the item is not present otherwise sections
  # that are not supposed to render will render.
  if (length(env) == 0) {
    return(NULL)
  }
  if (is.null(names(env)) || any(names(env) == "")) {
    rlang::abort("All elements of `env` must be named")
  }
  vars = sapply(names(env), function(name) paste0(toupper(name), "=\"", env[[name]], "\""))
  paste(vars, collapse = " ")
}

#' Check whether an image with the specified name exists on the system
#'
#' @param image_name Name of the image. Can optionally include a tag, like `"my-image:latest"`.
#' @param tag Optional image tag, i.e. `{image_name}:{tag}`. If a tag is provided in `image_name`
#'   it takes precedence and this argument is ignored.
#'
#' @return Boolean
#'
#' @keywords internal
lam_docker_image_exists <- function(image_name, tag = NULL) {
  if (grepl(":", image_name, fixed = TRUE)) {
    # Parse the tag out of the name
    name_split <- strsplit(image_name, ":", fixed = TRUE)[[1L]]
    image_name <- name_split[[1L]]
    tag <- name_split[[2L]]
  }
  if (is.null(tag)) {
    cmd <- glue::glue("docker image inspect {image_name} 2>&1 > /dev/null")
  } else {
    cmd <- glue::glue("docker image inspect {image_name}:{tag} 2>&1 > /dev/null")
  }
  exit_code <- try(lam_run_system_command(cmd, capture_output = FALSE, quiet = TRUE), silent = TRUE)
  exit_code == 0
}

# ---- .dockerignore ----

#' Write a `.dockerignore` file
#'
#' @keywords internal
write_dockerignore <- function() {
  file.copy(
    system.file("templates", "dockerignore", package = "lambdar", mustWork = TRUE),
    lam_dockerignore_path()
  )
  usethis::ui_done("Writing {usethis::ui_path(\".dockerignore\")}")
}
