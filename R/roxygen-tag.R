# Implementing the `@lambda` tag. See
# https://cran.r-project.org/web/packages/roxygen2/vignettes/extending.html

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_lambda <- function(x) {
  # We don't expect anything after the @lambda tag so there's not much to do here. We may want to
  # set `x$val` to something else in future
  x$val <- list(handler_file = x$file, handler_function = NULL)
  x
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
  parsed <- lapply(parsed, function(block) {
    for (i in seq_along(block$tags)) {
      if (block$tags[[i]]$tag == "lambda") {
        block$tags[[i]]$val$handler_function <- block$object$alias
      }
    }
    block
  })
  handlers <- lapply(parsed, lambdar_handler)
  unlist(lapply(handlers, as.character))
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

#' @keywords internal
lambdar_handler <- function(roxy_block = NULL, file = NULL, function_name = NULL) {
  if (!is.null(roxy_block)) {
    if (!inherits(roxy_block, "roxy_block")) {
      rlang::abort("`roxy_block` must be a roxy block object")
    }
    h <- get_handler_from_block(roxy_block)
    return(new_lambdar_handler(file = h[["file"]], function_name = h[["function_name"]]))
  }
  if (is.null(file) || is.null(function_name)) {
    rlang::abort("`file` and `function_name` must both be provided")
  }
  new_lambdar_handler(file = file, function_name = function_name)
}

#' @keywords internal
new_lambdar_handler <- function(file, function_name) {
  if (!lam_function_exists_in_file(file, function_name)) {
    msg <- glue::glue("No function named `{function_name}()` in `{file}`")
    rlang::abort(msg)
  }
  structure(list(file = file, function_name = function_name), class = c("lambdar_handler", "list"))
}

#' @export
as.character.lambdar_handler <- function(x, ...) {
  paste(gsub("\\.[Rr]$", "", x$file), x$function_name, sep = ".")
}

#' @export
print.lambdar_handler <- function(x, ...) {
  cat("<lambdar handler: ", as.character(x), ">\n", sep = "")
  cat("  File:     ", x$file, "\n", sep = "")
  cat("  Function: ", x$function_name, "()\n", sep = "")
}

#' Extract handler functions from a `roxy_block`
#'
#' @param block A `roxy_block`
#'
#' @return A named character vector containing `file` and `function_name` items.
#'
#' @keywords internal
get_handler_from_block <- function(block) {
  for (tag in block$tag) {
    if (tag$tag == "lambda") {
      return(c(file = tag$val$handler_file, function_name = tag$val$handler_function))
    }
  }
  NULL
}
