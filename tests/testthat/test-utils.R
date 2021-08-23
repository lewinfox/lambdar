test_that("`lam_build_quoted_list()` works correctly", {
  expect_equal(lam_build_quoted_list(c("foo", "bar")), "\"foo\", \"bar\"")
  expect_equal(lam_build_quoted_list(c("\"foo\"", "\'bar\'")), "\"foo\", \"bar\"")
  expect_equal(lam_build_quoted_list(c("\"\"foo\"\"", "\'\'bar\'\'")), "\"foo\", \"bar\"")
})

test_that("`lam_build_separated_list()` works correctly", {
  expect_equal(lam_build_separated_list(c("foo", "bar")), "foo bar")
  expect_equal(lam_build_separated_list(c("\"foo\"", "\'bar\'")), "foo bar")
  expect_equal(lam_build_separated_list(c("\"\"foo\"\"", "\'\'bar\'\'")), "foo bar")
  expect_equal(lam_build_separated_list(c("foo", "bar"), ", "), "foo, bar")
})

test_that("`lam_build_env_list()` works correctly", {
  expect_equal(lam_build_env_list(list(foo = "bar", baz = "boo")), "FOO=\"bar\" BAZ=\"boo\"")
})

test_that("file path helpers work", {
  with_local_project({
    file.create(lam_dockerfile_path()) # Otherwise `normalizePath()` fails
    root <- getwd()
    expect_equal(normalizePath(lam_proj_path()), root)
    expect_equal(normalizePath(lam_config_path()), file.path(root, "_lambdar.yml"))
    expect_equal(normalizePath(lam_runtime_path()), file.path(root, ".lambdar", "lambdar_runtime.R"))
    expect_equal(normalizePath(lam_dockerfile_path()), file.path(root, "Dockerfile"))
  })
})
