# --- AWS helpers ----

#' Shared params for AWS cli functions
#'
#' @param aws_account_id Your 12-digit AWS account id
#' @param aws_region Your AWS region, e.g. `"ap-southeast-2"`
#' @param aws_ecr_repository_name Chosen name for your AWS ECR repository. It is recommended that
#'   this be the same as your app name.
#' @param tag Optional image tag. If omited, "latest" will be used.
#'
#' @name aws-generic-params
#'
#' @keywords internal
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
    if (!lam_is_quiet()) {
      cli::cli_alert_info("Attempting to get AWS account ID")
    }
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
  # Check that the image exists
  image_exists <- lam_docker_image_exists(cfg$app_name)
  if (!image_exists) {
    rlang::abort("No Docker image found. Have you called `lambdar::build_image()`?")
  }
  tag <- lam_ecr_image_tag_name(
    aws_account_id = cfg$aws_account_id,
    aws_region = cfg$aws_region,
    aws_ecr_repository_name = cfg$app_name,
    tag = tag
  )
  docker_tag_cmd <- glue::glue("docker tag {cfg$app_name} {tag}")
  lam_run_system_command(docker_tag_cmd)
  if (!lam_is_quiet()) {
    cli::cli_alert_success("Suggessfully tagged {.code {cfg$app_name}} as {.code {tag}}")
  }
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
  if (!lam_is_quiet()) {
    cli::cli_alert_info("Creating the repository if it doesn't already exist")
  }
  cmd <- glue::glue(
    # If the describe command fails, the repo doesn't exist (or something else has gone wrong)
    paste(
      "aws ecr describe-repositories --repository-names {aws_ecr_repository_name} ||" ,
      "aws ecr create-repository --repository-name {aws_ecr_repository_name}"
    )
  )
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

  if (!lam_is_quiet()) {
    cli::cli_alert_info("Authenticating Docker with AWS ECR")
  }
  lam_run_system_command(authentication_cmd)
  image_tag <- lam_ecr_image_tag_name(aws_account_id, aws_region, aws_ecr_repository_name)

  upload_cmd <- glue::glue("docker push {image_tag}")
  if (!lam_is_quiet()) {
    cli::cli_alert_info("Uploading container image")
  }

  lam_run_system_command(upload_cmd)
  if (!lam_is_quiet()) {
    cli::cli_alert_success("Successfully uploaded {image_tag}")
  }
}
