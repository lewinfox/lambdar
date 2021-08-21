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


# ---- AWS ----

#' Shared params for AWS cli functions
#'
#' @param aws_account_id Your 12-digit AWS account id
#' @param aws_region Your AWS region, e.g. `"ap-southeast-2"`
#' @param aws_ecr_repository_name Chosen name for your AWS ECR repository. It is recommended that
#'   this be the same as your app name.
#' @param tag Optional image tag. If omited, "latest" will be used.
#'
#' @name aws-generic-params
NULL


#' Do we have the AWS cli installed?
#'
#' Examines the output of [Sys.which()] to see if we can locate an installation.
#'
#' @return Boolean
#'
#' @keywords internal
lam_has_aws_cli <- function() {
  if (Sys.which("aws") == "") {
    msg <- paste(
      "No AWS cli installation found.",
      "Refer to {.url https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-install.html}",
      "for installation instructions"
    )
    cli::cli_alert_warning(msg)
    return(FALSE)
  }
  TRUE
}

#' Attempt to get our AWS account ID
#'
#' @return A character string
#'
#' @keywords internal
lam_aws_get_account_id <- function() {
  if (lam_has_aws_cli()) {
    cli::cli_alert_info("Attempting to get AWS account ID")
    if (.Platform$OS.type == "unix") {
      aws_account_id <- lam_run_system_command("aws sts get-caller-identity --output text | cut -f 1", capture_output = TRUE)
      return(aws_account_id)
    } else {
      rlang::warn("AWS account ID collection is not implemented for Widows yet.")
    }
  }
  return("")
}

#' Create the AWS ECR tag for your image
#'
#' @inheritParams aws-generic-params
#' @return A character vector
#'
#' @keywords internal
lam_ecr_image_tag_name <- function(aws_account_id, aws_region, aws_ecr_repository_name, tag = "latest") {
  as.character(glue::glue("{aws_account_id}.dkr.ecr.{aws_region}.amazonaws.com/{aws_ecr_repository_name}:{tag}"))
}

#' Tag an image in preparation for upload to AWS Lambda
#'
#' See [this blog post](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/)
#' for details.
#'
#' @inheritParams aws-generic-params
#'
#' @keywords internal
lam_ecr_tag_image_for_upload <- function(tag = "latest") {
  if (!lam_has_docker()) {
    rlang::abort("No Docker installation detected")
  }
  cfg <- lambdar_config_from_file()
  tag <- lam_ecr_image_tag_name(
    aws_account_id = cfg$aws_account_id,
    aws_region = cfg$aws_region,
    aws_ecr_repository_name = cfg$app_name,
    tag = tag
  )
  docker_tag_cmd <- glue::glue("docker tag {cfg$app_name} {tag}")
  lam_run_system_command(docker_tag_cmd)
  cli::cli_alert_success("Suggessfully tagged {.code {cfg$app_name}} as {.code {tag}}")
}

#' Generate an ECR repo URL
#'
#' @inheritParams aws-generic-params
#'
#' @keywords internal
lam_ecr_repo_url <- function(aws_account_id, aws_region, aws_ecr_repository_name) {
  as.character(glue::glue("{aws_account_id}.dkr.ecr.{aws_region}.amazonaws.com"))
}

#' Create an ECR repository if it doesn't already exist
#'
#' @inheritParams aws-generic-params
#'
#' @keywords internal
lam_ecr_create_repo_if_not_exists <- function(aws_ecr_repository_name) {
  cli::cli_alert_info("Creating the repository if it doesn't already exist")
  cmd <- glue::glue("aws ecr create-repository --repository-name {aws_ecr_repository_name}")
  lam_run_system_command(cmd)
}

#' Upload an image to ECR
#'
#' @inheritParams aws-generic-params
#'
#' @keywords internal
lam_ecr_upload_image <- function(aws_account_id, aws_region, aws_ecr_repository_name) {
  if (!lam_has_docker() || !lam_has_aws_cli()) {
    rlang::abort("Both Docker and the AWS cli are required to upload a container image")
  }
  ecr_repo_url <- lam_ecr_repo_url(aws_account_id, aws_region, aws_ecr_repository_name)
  lam_ecr_create_repo_if_not_exists(aws_ecr_repository_name)
  authentication_cmd <- glue::glue("aws ecr get-login-password | docker login --username AWS --password-stdin {ecr_repo_url}")
  cli::cli_alert_info("Authenticating Docker with AWS ECR")
  lam_run_system_command(authentication_cmd)
  image_tag <- lam_ecr_image_tag_name(aws_account_id, aws_region, aws_ecr_repository_name)
  upload_cmd <- glue::glue("docker push {image_tag}")
  cli::cli_alert_info("Uploading container image")
  lam_run_system_command(upload_cmd)
  cli::cli_alert_success("Successfully uploaded {image_tag}")
}

# ---- Path utilities ----

#' Path utilities
#'
#' Pinched from `usethis`.
#'
#' @param ... Path elements
#'
#' @keywords internal
lam_proj_path <- function(dir = ".", ...) {
    relish(file.path(dir, ...))
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
  msg <- glue::glue("Symbol `{fun}` exists in `{file}` but is not a function")
  rlang::warn(msg)
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

#' Invoke a system command and check to see if it was successful
#'
#' This is a wrapper around [system()] with some extra bells and whistles.
#'
#' @param cmd String, a system command
#' @param capture_output Do we want the actual output of the command returned? Useful when trying to
#'   generate text. This is passed directly to tne `intern` parameter of [system()].
#'
#' @return If `capture_output` is `FALSE`, invisibly returns the exit code of the command, as long
#'   as that is zero. Any other exit code throws an error. If `capture_output` is `TRUE`, returns
#'   whatever the text output of the command is.
lam_run_system_command <- function(cmd, capture_output = FALSE) {
  cli::cli_alert("{.code {cmd}}")
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
