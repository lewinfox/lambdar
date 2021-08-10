lambdar_config <- function(config_file) {
  if (!file.exists(config_file)) {
    cli::cli_alert_danger("File {.path {config_file}} does not exist")
    rlang::abort("File not found", "lambdar_no_config")
  }
  cfg <- yaml::read_yaml(config_file)
  new_lambdar_config(cfg)
}

new_lambdar_config <- function(cfg) {
  if (!is.list(cfg)) {
    msg <- glue::glue("`cfg` must be a list, not {typeof(cfg)}")
    rlang::abort(msg)
  }
  # Extract handler information and make sure it can be found in the specified file
  split_handler <- strsplit(cfg$lambda_handler, ".", fixed = TRUE)[[1]]
  cfg$r_handler_file <- relish(file.path(usethis::proj_get(), paste0(split_handler[[1]], ".R")))
  if (!file.exists(cfg$r_handler_file)) {
    cli::cli_alert_warning("File {.path {cfg$r_handler_file}} could not be found.")
    rlang::abort("Handler file not found")
  }
  cfg$r_handler_function <- split_handler[[2]]
  if (!lam_function_exists_in_file(cfg$r_handler_file, cfg$r_handler_function)) {
    cli::cli_alert_danger("Function {.fn {cfg$r_handler_function}} was not found in {.path {cfg$r_handler_file}}")
    rlang::abort("Handler function not found in file")
  }
  structure(cfg, class = c("lambdar_config", class(cfg)))
}

#' Check that a file contains a function
#'
#' @param file A file path
#' @param fun String. A function that we expect to be defined in `file`
#'
#' @return Boolean
lam_function_exists_in_file <- function(file, fun) {
  local(
    {
      source(file)
      exists <- exists(fun)
      if (!exists) {
        return(FALSE)
      }
      is_fun <- is.function(eval(parse(text = fun)))
      if (!is_fun) {
        cli::cli_alert_warning("{.var {fun}} is not a function in {.path {file}}")
        return(FALSE)
      }
      TRUE
    }
  )
}
