

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

#' @describeIn lam_proj_path Path to `.dockerignore`
lam_dockerignore_path <- function() {
  lam_proj_path(".dockerignore")
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
#' Uses [renv::dependencies()] to work out what packages we depend on. Ignores `rmarkdown` where
#' that is triggered by the presence of markdown files, and also excludes packages that are already
#' in the container because the runtime needs them (`httr`, `jsonlite` and `logger`).
#'
#' @return A character vector of R package dependencies needed by `file`
#'
#' @keywords internal
lam_get_project_dependencies <- function() {

  deps <- renv::dependencies(lam_proj_path(), progress = FALSE)

  # Ignore any dependencies on `rmarkdown` where the source is itself a markdown file.
  # `renv::dependencies()` wants us to install `rmarkdown` so it can parse these, but in a container
  # context that's very unlikely to be helpful and it's quite a heavy dependency to put into an
  # image.
  deps <- deps[!(grepl("\\.[Rr]md$", deps$Source) & deps$Package == "rmarkdown"), "Package"]

  # TODO: Is there a neater way of excluding runtime dependencies?
  setdiff(unique(deps), c("httr","jsonlite", "logger"))
}

#' Scan the project folder for handlers and package dependencies
#'
#' @return A list containing two named items, `lambda_handlers` and `r_packages`,
#'   each of which is a character vector, possible of length zero.
#'
#' @keywords internal
lam_scan_project <- function() {
  quiet <- lam_is_quiet()

  if (!quiet) {
    cli::cli_alert_info("Scanning project...")
  }

  handlers <- lam_parse_project_handlers()

  if (!quiet) {
    if (length(handlers) > 0) {
      msg <- "Found {length(handlers)} handler function{?s}"
      cli::cli_alert_success(msg)
    } else {
      msg <- paste("No handler functions found.",
                   "Tag functions with {.code #' @lambda} so lambdar can find them.")
      cli::cli_alert_info(msg)
    }

  }

  r_packages <- lam_get_project_dependencies()

  if (!quiet && length(r_packages) > 0) {
    cli::cli_alert_success("Detected {length(r_packages)} R package dependenc{?y/ies}.")
  }

  list(
    lambda_handlers = handlers,
    r_packages = r_packages
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

#' What operating system are we on?
#'
#' Needed to warn Windows users
#'
#' @return One of `"osx"`, `"linux"` or `"windows"`.
#'
#' @keywords internal
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

lam_is_quiet <- function() {
  getOption("lambdar.quiet")
}


# ---- Execute system command ----

#' Invoke a system command and check to see if it was successful
#'
#' This is a wrapper around [system()] with some extra bells and whistles.
#'
#' @param cmd String, a system command
#' @param capture_output Do we want the actual output of the command returned? Useful when trying to
#'   generate text. This is passed directly to tne `intern` parameter of [system()].
#' @param quiet Set to `FALSE` to reduce verbosity of lambdar messages. Note that system commands
#'   may still write to the terminal.
#'
#' @return If `capture_output` is `FALSE`, invisibly returns the exit code of the command, as long
#'   as that is zero. Any other exit code throws an error. If `capture_output` is `TRUE`, returns
#'   whatever the text output of the command is.
#'
#' @keywords internal
lam_run_system_command <- function(cmd, capture_output = FALSE, quiet = lam_is_quiet()) {
  if (!quiet) {
    cli::cli_alert("{.code {cmd}}")
  }
  res <- system(cmd, intern = capture_output)
  if (!capture_output) {
    if (res != 0) {
      rlang::abort(
        "Execution of system command failed",
        class = "lambdar_system_cmd_failed",
        exit_code = res,
        cmd = cmd
      )
    }
  }
  if (capture_output) {
    return(res)
  }
  invisible(res)
}
