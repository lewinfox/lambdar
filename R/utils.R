

# ---- Path utilities ----

#' Path utilities
#'
#' Pinched from `usethis`.
#'
#' @param ... Path elements
#'
#' @keywords internal
lam_proj_path <- function(..., ext = "") {
    relish(usethis::proj_path(..., ext = ext))
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

#' Scan the project folder for handlers and package dependencies
#'
#' @return A list containing three named items, `lambda_handlers`, `include_files` and `r_packages`,
#'   each of which is a character vector, possible of length zero.
#'
#' @keywords internal
lam_scan_project <- function() {
  quiet <- lam_is_quiet()

  if (!quiet) {
    cli::cli_alert_info("Scanning project...")
  }

  # TODO: Look for source() calls and follow that graph.
  include_files <- lam_handler_filenames()
  handlers <- lam_parse_project_handlers()

  if (!quiet) {
    if (length(include_files) > 0 && length(handlers) > 0) {
      msg <- "Found {length(handlers)} handler function{?s} in {length(include_files)} file{?s}"
      cli::cli_alert_success(msg)
    } else {
      msg <- paste("No handler functions found.",
                   "Tag functions with {.code #' @lambda} so lambdar can find them.")
      cli::cli_alert_info(msg)
    }

  }

  r_packages <- lam_get_file_dependencies(include_files)

  if (!quiet && length(r_packages) > 0) {
    cli::cli_alert_success("Detected {length(r_packages)} R package dependenc{?y/ies}.")
  }

  list(
    lambda_handlers = handlers,
    include_files = include_files,
    r_packages = r_packages
  )
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
  dir.exists(lam_dir_path()) && runtime_exists()
}

#' @describeIn using_lambdar Does the runtime file exist?
runtime_exists <- function() {
  file.exists(lam_runtime_path())
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
  !inherits(try(usethis::ui_silence(usethis::proj_path()), silent = TRUE), "try-error")
}
