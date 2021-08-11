test_that("`@lambda` tag is parsed correctly", {
  text <- "
  #' @lambda
  f <- function(x, y) {
    TRUE
  }

  #' @lambda
  g <- function() {
    FALSE
  }
  "
  # Roxygen should have no trouble parsing this code
  expect_length(roxygen2::parse_text(text), 2)
})
