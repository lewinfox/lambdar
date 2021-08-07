#' Tidy file paths by removing instances of `dir` from `x`.
#'
#' Ensures that we get a relative path for `x`.
#'
#' @param x File path
#' @param dir Directory to strip from `x`
relish <- function(x, dir = getwd()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }
  gsub(dir, "", x, fixed = TRUE)
}
