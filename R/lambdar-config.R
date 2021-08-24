# ---- Config S3 class ----

#' @describeIn lambdar_config_from_file Create a new `lambdar_config` object. For internal use only.
#'
#' @param config A named list containing params. If `config` or any elements are missing, defaults
#'   will be used.
new_lambdar_config <- function(config = NULL) {

  if (!is.null(config)) {
    if (!is.list(config)) {
      msg <- glue::glue("`config` must be a list, not {typeof(config)}")
      rlang::abort(msg, "lambdar_bad_config")
    }
    if (any(names(config) == "")) {
      rlang::abort("All elements of `config` must be named.", "lambdar_bad_config")
    }
  }

  # Build the default app name by using the project folder name. Clean up the path to replace any
  # non-word characters, lowercase etc. Generally try and make it presentable.
  app_name <- tolower(stringr::str_replace_all(basename(usethis::proj_path()), "\\W+$?", "-"))
  app_name <- stringr::str_remove(app_name, "\\W+$") # No trailing non-word chars
  app_name <- stringr::str_remove(app_name, "^\\W+") # No leading non-word chars

  # This is the default config object. We have the option of overwriting additional parameters by
  # passing a list into the `config` argument

  # If not configured, the default value of `getOption("repos")` is `@CRAN@`, so we need to replace
  # that
  r_package_repos <- unname(getOption("repos"))
  r_package_repos <- stringr::str_replace_all(
    r_package_repos,
    "^@CRAN@$",
    "https://cran.r-project.org"
  )

  cfg <- list(
    app_name =  app_name,              # Defaults to "$USER/$PROJECT_DIR"
    r_version = lam_r_version(),       # Current session's R version, e.g. "4.0.1"
    lambda_handlers = NULL,            # In the format "file.function_name"
    r_packages = NULL,                 # R packages to be installed in the image
    r_package_repos = r_package_repos, # See above
    linux_packages = NULL,             # Linux packages to be installed in the image
    env = NULL,                        # Environment variables to be set in the container
    lambdar_version = as.character(utils::packageVersion("lambdar"))
  )

  # Overwrite `cfg` with any values supplied in `config`
  if (is.list(config)) {
    cfg <- utils::modifyList(cfg, config, keep.null = TRUE)
  }

  structure(cfg, class = c("lambdar_config", "list"))
}

#' @describeIn lambdar_config_from_file Test if an object is a `lambdar_config` object.
is_lambdar_config <- function(x) {
  inherits(x, "lambdar_config")
}

# ---- S3 methods ----

#' @export
print.lambdar_config <- function(x, ...) {
  cat("<lambdar config object>\n\n")
  NextMethod()
}

# ---- Config file ----

#' Read a YAML file and return a config object
#'
#' @return A [lambdar_config] object.
read_config <- function() {
  config_file <- lam_config_path()
  if (!file.exists(config_file)) {
    rlang::abort("No config file found in project", "lambdar_no_config_file")
  }
  cfg <- yaml::read_yaml(lam_config_path())
  new_lambdar_config(cfg)
}

#' Create a new lambdar config object by reading the config file
#'
#' @return A [lambdar_config] object.
#'
#' @keywords internal
#'
#' @aliases lambdar_config
lambdar_config_from_file <- function() {
  cfg <- read_config()
  new_lambdar_config(cfg)
}

#' Take a config object, format its contents and write the config file
#'
#' Formatting involves correctly quoting R vectors for inclusion in the config file.
#'
#' @param cfg A [lambdar_config] object.
#'
#' @keywords internal
write_config <- function(cfg) {
  if (!is_lambdar_config(cfg)) {
    msg <- glue::glue("`cfg` must be a lambdar_config object, not {class(cfg)}")
    rlang::abort(msg)
  }

  # Formatting
  cfg$lambda_handlers <- lam_build_quoted_list(cfg$lambda_handlers)
  cfg$r_packages      <- lam_build_separated_list(cfg$r_packages, sep = ", ")
  cfg$r_package_repos <- lam_build_quoted_list(cfg$r_package_repos)
  cfg$linux_packages  <- lam_build_separated_list(cfg$linux_packages, sep = ", ")

  # TODO: This won't work in non-interactive sessions where the file exists already because
  #       `use_template()` avoids clobbering the file if it can't check with the user.
  usethis::use_template("_lambdar.yml", data = cfg, package = "lambdar")
}
