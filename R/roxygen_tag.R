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
    parsed <- roxygen2::parse_file(file)
    res <- c(
      res,
      sapply(
        parsed,
        function(lambda) paste(gsub("\\.[Rr]$", "", lambda$file), lambda$object$alias, sep = ".")
      )
    )
  }
  res
}
