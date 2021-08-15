test_that("`lam_build_quoted_list()` works correctly", {
  expect_equal(lam_build_quoted_list(c("foo", "bar")), "\"foo\", \"bar\"")
  expect_equal(lam_build_quoted_list(c("\"foo\"", "\'bar\'")), "\"foo\", \"bar\"")
  expect_equal(lam_build_quoted_list(c("\"\"foo\"\"", "\'\'bar\'\'")), "\"foo\", \"bar\"")
})

test_that("`lam_build_space_separated_list()` works correctly", {
  expect_equal(lam_build_space_separated_list(c("foo", "bar")), "foo bar")
  expect_equal(lam_build_space_separated_list(c("\"foo\"", "\'bar\'")), "foo bar")
  expect_equal(lam_build_space_separated_list(c("\"\"foo\"\"", "\'\'bar\'\'")), "foo bar")
})

test_that("`lam_build_env_list()` works correctly", {
  expect_equal(lam_build_env_list(list(foo = "bar", baz = "boo")), "FOO=\"bar\" BAZ=\"boo\"")
})
