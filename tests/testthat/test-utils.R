test_that("`lam_find_sourced_files()` works", {
  text <- "
  source('foo.R')
  source(\"bar.R\")
  x <- 2
  "
  withr::with_file("test.R", {
    writeLines(text, "test.R")
    expect_equal(lam_find_sourced_files("test.R"), c("foo.R", "bar.R"))
  })

  # Warning expected when sourced files contain dir separators
  text <- "
  source('foo.R')
  source(\"foo/bar.R\")
  source(\"foo\\bar.R\") # Windows
  x <- 2
  "
  withr::with_file("test.R", {
    writeLines(text, "test.R")
    expect_warning(lam_find_sourced_files("test.R"), regexp = "bar.R")
  })
})
