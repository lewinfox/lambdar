#' Tidy file paths by removing instances of `dir` from `x`.
#'
#' Ensures that we get a relative path for `x`. Pinched from `rlang:::relish()`.
#'
#' @param x File path
#' @param dir Directory to strip from `x`
relish <- function(x, dir = getwd()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  gsub(dir, "", x, fixed = TRUE)
}

#' Convert a vector into a list
#'
#' @param items Vector or items to write.
#'
#' @return A string
#'
#' @name build-lists
#' @keywords internal
NULL

#' @describeIn build-lists Build a single-quoted comma-separated list
lam_build_quoted_list <- function(items = NULL) {
  if (is.null(items)) {
    return(NULL)
  }
  glue::glue_collapse(glue::single_quote(items), sep = ",")
}

#' @describeIn build-lists Build an unquoted space-separated list
lam_build_space_separated_list <- function(items = NULL) {
  if (is.null(items)) {
    return(NULL)
  }
  glue::glue_collapse(items, sep = " ")
}

#' Get the current R version
#'
#' @return Current R version as a string, e.g. "4.0.1"
#' @keywords internal
lam_r_version <- function() {
  paste0(R.version$major, ".", R.version$minor)
}
