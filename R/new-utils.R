# ---- Read and write files ----
read_config <- function() {
  config_file <- lam_config_path()
  if (!file.exists(config_file)) {
    rlang::abort("No config file found in project", "lambdar_no_config_file")
  }
  cfg <- yaml::read_yaml(lam_config_path())
  new_lambdar_config(cfg)
}

write_config <- function(cfg) {
  if (!is_lambdar_config(cfg)) {
    msg <- glue::glue("`cfg` must be a lambdar_config object, not {class(cfg)}")
    rlang::abort(msg)
  }
  usethis::use_template("_lambdar.yml", data = cfg, package = "lambdar")
}

write_dockerfile <- function(cfg) {
  if (!is_lambdar_config(cfg)) {
    msg <- glue::glue("`cfg` must be a lambdar_config object, not {class(cfg)}")
    rlang::abort(msg)
  }
  usethis::use_template("Dockerfile", data = cfg, package = "lambdar")
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

# ---- Misc ----

lam_is_quiet <- function() {
  getOption("lambdar.quiet")
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
