# ---- Dockerfile formatting helpers ----

#' Convert a vector into a list
#'
#' @param items Vector or items to write.
#'
#' @return A string
#'
#' @name build-lists
#' @keywords internal
NULL

#' @describeIn build-lists Build a single-quoted comma-separated list
lam_build_quoted_list <- function(items = NULL) {
  if (is.null(items)) {
    return(NULL)
  }
  glue::glue_collapse(glue::double_quote(items), sep = ", ")
}

#' @describeIn build-lists Build an unquoted space-separated list
lam_build_space_separated_list <- function(items = NULL) {
  if (is.null(items)) {
    return(NULL)
  }
  glue::glue_collapse(items, sep = " ")
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


# ---- Check Docker ----

#' Do we have docker installed?
#'
#' @keywords internal
lam_has_docker <- function() {
  x <- Sys.which("docker")
  if (x == "") {
    msg <- paste(
      "No docker installation found.",
      "Refer to {.url https://docs.docker.com/get-docker/} for installation instructions"
    )
    cli::cli_alert_warning(msg)
    return(FALSE)
  }
  TRUE
}

# ---- Path utilities ----

#' Path utilities
#'
#' Pinched from `usethis`.
#'
#' @param ... Path elements
#'
#' @keywords internal
lam_proj_path <- function(...) {
  relish(file.path(usethis::proj_get(), ...))
}

#' @describeIn lam_proj_path Path to the `.lambdar/` directory
lam_dir_path <- function() {
  lam_proj_path(".lambdar")
}

#' @describeIn lam_proj_path Path to `_lambdar.yml` config file
lam_config_file <- function() {
  lam_proj_path("_lambdar.yml")
}

#' @describeIn lam_proj_path Path to Dockerfile
lam_dockerfile_path <- function() {
  lam_proj_path("Dockerfile")
}

#' @describeIn lam_proj_path Path to runtime function
lam_runtime_path <- function() {
  lam_proj_path(lam_dir_path(), "lambdar_runtime.R")
}

#' Tidy file paths by removing instances of `dir` from `x`.
#'
#' Ensures that we get a relative path for `x`. Pinched from `rlang:::relish()`.
#'
#' @param x File path
#' @param dir Directory to strip from `x`
#'
#' @return Character vector
#'
#' @keywords internal
relish <- function(x, dir = usethis::proj_get()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  gsub(dir, "", x, fixed = TRUE)
}
