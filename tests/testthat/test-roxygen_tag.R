test_that("`@lambda` tag is parsed correctly", {

  if (!requireNamespace("withr", quietly = TRUE)) {
    skip("Package `withr` is required but not installed")
  }

  text <- "
  #' @lambda
  f <- function(x, y) {
    TRUE
  }

  #' @lambda
  g <- function() {
    FALSE
  }

  #' More complete documentation
  #'
  #' This is longer than the short docs above
  #'
  #' @param foo A bar
  #'
  #' @return A baz
  #'
  #' @export
  #'
  #' @lambda
  foobar <- function(foo) {
    baz(foo)
  }
  "

  # `lam_parse_file_handlers()` should return a vector of "file.function_name"
  withr::with_tempfile(
    "tmp",
    {
      cat(text, file = tmp)
      expect_equal(lam_parse_file_handlers(tmp), paste(tmp, c("f", "g", "foobar"), sep = "."))
    }
  )

})

test_that("functions without the `@lambda` tag are not detected", {

  if (!requireNamespace("withr", quietly = TRUE)) {
    skip("Package `withr` is required but not installed")
  }

  text <- "
  #' No @lambda tag
  #'
  #' @param foo A bar
  #'
  #' @export
  f <- function(foo) {
    bar(foo)
  }


  # No docs of any kind

  g <- function() {
    FALSE
  }

  # A non-compliant roxygen block

  # @lambda
  barbaz <- function() {
    FALSE
  }

  # Tag with no function

  #' @lambda

  #' This is the only one we should find
  #'
  #' @lambda
  findme <- function() {
    TRUE
  }
  "

  withr::with_tempfile(
    "tmp",
    {
      cat(text, file = tmp)
      expect_equal(lam_parse_file_handlers(tmp), paste(tmp, c("findme"), sep = "."))
    }
  )
})
