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
  # Testing text processing
  roclet_proc_text_result <- roxygen2::roc_proc_text(roclet_lambda(), text)

  expect_type(roclet_proc_text_result, "list")
  expect_length(roclet_proc_text_result, 2)

  # Testing processing the output of the function above
  roclet_output_result <- suppressMessages(roxygen2::roclet_output(roclet_lambda(), roclet_proc_text_result))

  expect_type(roclet_output_result, "list")
  expect_length(roclet_output_result, 2)
  expect_message(roxygen2::roclet_output(roclet_lambda(), roclet_proc_text_result), "I got a lambda roclet")
})
