# Implementing the `@lambda` tag. See
# https://cran.r-project.org/web/packages/roxygen2/vignettes/extending.html

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_lambda <- function(x) {
  # We don't expect anything after the @lambda tag so there's not much to do here. We may want to
  # set `x$val` to something else in future
  x$val <- "Function to be exported to AWS lambda"
  x
}

#' Roclet lambda class
#'
#' Create an object of class `roclet_lambda` to ensure the correct S3 methods are called.
#'
#' @keywords internal
roclet_lambda <- function() {
  roxygen2::roclet("lambda")
}

#' Extracts functions to be lambda-d from the roxygen block/s
#'
#' @param x A `roclet_lambda` object. See [roclet_lambda()].
#' @param blocks A list of [roxygen2::roxy_block]s. Each block corresponds to the captured into of
#'   one function.
#' @param env An environment - not sure what this does.
#' @param base_path Not sure about this either.
#'
#' @importFrom roxygen2 roclet_process
#' @export
#' @keywords internal
roclet_process.roclet_lambda <- function(x, blocks, env, base_path) {
  res <- list()
  for (block in blocks) {
    nm <- block[["object"]][["alias"]]
    fn <- block[["object"]][["value"]]
    res[[nm]] <- fn
  }
  res
}

#' Do stuff with parsed lambda functions
#'
#' Given a list of functions that have been tagged with `@lambda`, do something cool with them
#'
#' @param x A `roclet_lambda` object. See [roclet_lambda()].
#' @param results The output of [roclet_process.roclet_lambda].
#' @param base_path Unknown.
#' @param ... Unused
#'
#' @importFrom roxygen2 roclet_output
#' @export
#' @keywords internal
roclet_output.roclet_lambda <- function(x, results, base_path, ...) {
  message("I got a lambda roclet")
  results
}

#' Search project files for `@lambda` tags
#'
#' Searches the project root directory for `.R` files containing functions tagged with `@lambda` and
#' returns a character vector of AWS Lambda handler specifiers in the format `"file.function"`.
#'
#' @return Returns a character vector of `"file.function"` handler specifications.
#'
#' @keywords internal
lam_parse_project_handlers <- function() {
  files <- list.files(path = lam_proj_path(), pattern = "\\.R$", all.files = FALSE)
  res <- character()
  for (file in files) {
    # TODO: Do this without using c() all the time. Not the end of the world as we don't expect
    #       large numbers of lambda functions, but this is still a bit gross.
    res <- c(res, lam_parse_file_handlers(file))
  }
  res
}

#' Parse files containing `@lambda` tags into AWS Lambda handler specifiers
#'
#' AWS Lambda handlers are specified as `file.function_name`.
#'
#' @param file A file path
#'
#' @return Character vector containing `"file.function_name"` for any `@lambda`-tagged functions in
#'   `file`.
#'
#' @keywords internal
lam_parse_file_handlers <- function(file) {
  parsed <- roxygen2::parse_file(file)
  has_lambda_tag <- sapply(parsed, lam_roxy_block_has_lambda_tag)
  if (!any(has_lambda_tag)) {
    return(invisible(NULL))
  }
  parsed <- parsed[has_lambda_tag]
  sapply(
    parsed,
    function(block) paste(gsub("\\.[Rr]$", "", block$file), block$object$alias, sep = ".")
  )
}

#' Does a parsed roxygen block contain a `@lambda` tag?
#'
#' @param block A roxy_block from [roxygen2::parse_file()]
#'
#' @return `TRUE` if the block contains a `@lambda` tag, `FALSE` otherwise.
#'
#' @keywords internal
lam_roxy_block_has_lambda_tag <- function(block) {
  # TODO: Stop suppressing warnings like this! Not sure what it is that generates them, probably
  #       need to read the docs
  #       (https://cran.r-project.org/web/packages/roxygen2/vignettes/extending.html)
  #'      to understand how to use this properly, but it works for now.
  tryCatch(
    {
      any(sapply(block$tags, function(tag) tag$tag == "lambda"))
    },
    warning = function(e) {}
  )
}
