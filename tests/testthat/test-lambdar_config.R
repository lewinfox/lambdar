test_that("handler function detection works correctly", {
  f <- tempfile()
  fn <- "
  find_me <- function() TRUE

  not_a_function <- 1
  "
  cat(fn, file = f)
  expect_true(lam_function_exists_in_file(f, "find_me"))
  expect_false(lam_function_exists_in_file(f, "dont_find_me"))
  expect_message(lam_function_exists_in_file(f, "not_a_function"), regexp = "not a function")
})
