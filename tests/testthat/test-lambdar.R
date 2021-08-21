test_that("`init()` creates the correct directory structure", {
  skip_on_cran()
  skip("Not ready yet")

  project_path <- suppressMessages(create_local_thing(thing = "project"))


  withr::with_dir(project_path, {
    suppressMessages({
      expect_false(file.exists(lam_config_path()))
      expect_false(dir.exists(lam_dir_path()))
      init()
      expect_true(file.exists(lam_config_path()))
      expect_true(dir.exists(lam_dir_path()))
      expect_true(file.exists(lam_runtime_path()))
    })
  })
})
