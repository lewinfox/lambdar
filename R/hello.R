#' @lambda
hello_world <- function(name = NULL) {
  if (is.null(name)) {
    name <- "World"
  }
  paste("Hello,", name)
}
