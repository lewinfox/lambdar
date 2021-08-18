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
lam_build_space_separated_list <- function(items = NULL) {
  # The templating function needs to receive NULLs if the item is not present otherwise sections
  # that are not supposed to render will render.
  if (is.null(items)) {
    return(NULL)
  }
  items <- stringr::str_remove_all(items, "[\"\']")
  as.character(glue::glue_collapse(items, sep = " "))
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


# ---- Docker ----

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

lam_docker_container_tag_name <- function(aws_id, aws_region, repository_name, tag = NULL) {
  if (is.null(tag)) {
    tag <- "latest"
  }
  as.character(glue::glue("{aws_id}.dkr.ecr.{region}.amazonaws.com/{repository_name}:{tag}"))
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
    relish(file.path(usethis::proj_path(), ...))
}

#' @describeIn lam_proj_path Path to the `.lambdar/` directory
lam_dir_path <- function() {
  lam_proj_path(".lambdar")
}

#' @describeIn lam_proj_path Path to `_lambdar.yml` config file
lam_config_path <- function() {
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
relish <- function(x, dir = getwd()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  gsub(dir, "", x, fixed = TRUE)
}

# ---- Dependency detection ----

#' Detect R package dependencies in code
#'
#' @param file Path to a file or files to analyse
#'
#' @return A character vector of R package dependencies needed by `file`
#'
#' @keywords internal
lam_get_file_dependencies <- function(file) {
  if (length(file) > 1) {
    unique(sapply(file), lam_get_file_dependencies)
  }
  deps <- renv::dependencies(file, progress = FALSE)
  unique(deps$Package)
}

#' Get a list of all `.R` files containing `@lambda` handlers
#'
#' @return Character vector of file paths
#'
#' @keywords internal
lam_handler_filenames <- function() {
  handlers <- lam_parse_project_handlers()
  unique(
    sapply(
      strsplit(handlers, ".", fixed = T),
      function(f) paste0(f[[1]], ".R")
    )
  )
}

# ---- Misc ----

#' Check that a file contains a function
#'
#' @param file A file path
#' @param fun String. A function that we expect to be defined in `file`
#'
#' @return Boolean
#'
#' @keywords internal
lam_function_exists_in_file <- function(file, fun) {
  if (!file.exists(file)) {
    rlang::abort(glue::glue("File `{file}` does not exist"))
  }
  # Source the file into a new env and check that the named function exists
  e <- new.env(parent = baseenv())
  source(file, local = e)
  exists <- exists(fun, envir = e)
  if (!exists) {
    return(FALSE)
  }
  is_fun <- is.function(eval(parse(text = fun), envir = e))
  if (!is_fun) {
    return(FALSE)
  }
  TRUE
}

#' Have we got everything we need?
#'
#' @return Boolean
#'
#' @keywords internal
using_lambdar <- function() {
  dir.exists(lam_dir_path())
}

#' @describeIn using_lambdar Does the config file exist?
config_exists <- function() {
  file.exists(lam_config_path())
}

#' @describeIn using_lambdar Does the Dockerfile exist?
dockerfile_exists <- function() {
  file.exists(lam_dockerfile_path())
}

#' Are we in a project-y environment?
#'
#' @return Boolean
#'
#' @keywords internal
in_project <- function() {
  !inherits(try(usethis::proj_path(), silent = TRUE), "try-error")
}
