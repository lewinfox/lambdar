test_that("handler function detection works correctly", {
  fn <- "
  find_me <- function() TRUE

  not_a_function <- 1
  "

  withr::with_tempfile("f", {
    cat(fn, file = f)
    expect_true(lam_function_exists_in_file(f, "find_me"))
    expect_false(lam_function_exists_in_file(f, "dont_find_me"))
    expect_false(lam_function_exists_in_file(f, "not_a_function"))
  })
})

test_that("`new_lambdar_config()` works as expected", {
  if (!in_project()) {
    skip("Skipping tests that must be run in a project")
  }

  # Make sure the class is correct
  expect_s3_class(new_lambdar_config(), "lambdar_config")

  # Accepting a list as input overrides default params
  expect_null(new_lambdar_config()$env)
  expect_equal(new_lambdar_config(list(env = list(foo = "bar")))$env$foo, "bar")

  # Throw an error on non-list input
  expect_error(new_lambdar_config(TRUE), regexp = "logical")
  expect_error(new_lambdar_config(1L:2L), regexp = "integer")

  # Throw an error if anything is unnamed
  expect_error(new_lambdar_config(list(a = 1, 2)), regexp = "must be named")
})
