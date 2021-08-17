#' Lambdar configuration object
#'
#' Lambdar stores information about your app in a configuration object of class `lambdar_config`.
#' This is just a named list.
#'
#' @param config_file Path to config file. If not provided, looks for `_lambdar.yml` in the project
#'   root durectory.
#'
#' @return A list.
#'
#' @keywords internal
#' @aliases lambdar_config
lambdar_config_from_file <- function(config_file = NULL) {
  # If no file path is supplied, use the default ($PROJECT_ROOT/_lambdar.yml)
  if (is.null(config_file)) {
    config_file <- lam_config_path()
  }

  if (!config_exists()) {
    build_config()
  }

  # Read the YAML
  cfg <- yaml::read_yaml(config_file)
  new_lambdar_config(cfg)
}

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

  user <- Sys.getenv("USER")
  proj_root_name <- basename(usethis::proj_path())
  app_name <- glue::glue("{user}/{proj_root_name}")

  # This is the default config object. We have the option of overwriting additional parameters by
  # passing a list into the `config` argument

  # The default value of `getOption("repos")` is `@CRAN@`, so we need to replace that
  r_package_repos <- unname(getOption("repos"))
  if (identical(r_package_repos, "@CRAN@")) {
    r_package_repos <- "https://cran.r-project.org"
  }

  r_package_repos <- lam_build_quoted_list(r_package_repos)

  cfg <- list(
    app_name =  app_name,              # Defaults to "$USER/$PROJECT_DIR"
    r_version = lam_r_version(),       # Current session's R version, e.g. "4.0.1"
    include_files = NULL,              # Files to be included in the container image
    lambda_handlers = NULL,            # In the format "file.function_name"
    r_packages = NULL,                 # R packages to be installed in the image
    r_package_repos = r_package_repos, # See above
    linux_packages = NULL,             # Linux packages to be installed in the image
    env = NULL                         # Environment variables to be set in the container
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

#' @export
print.lambdar_config <- function(x, ...) {
  cat("<lambdar config object>\n\n")
  NextMethod()
}
