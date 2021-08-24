test_that("`init()` creates the right project structure", {
  with_local_project({
    expect_true(file.exists(lam_runtime_path()))
    expect_true(file.exists(lam_config_path()))
    expect_true(file.exists(lam_dockerignore_path()))
  })
})

test_that("`clean()` removes everything", {
  with_local_project({
    expect_true(file.exists(lam_runtime_path()))
    expect_true(file.exists(lam_config_path()))
    expect_true(file.exists(lam_dockerignore_path()))
    clean()
    expect_false(file.exists(lam_runtime_path()))
    expect_false(file.exists(lam_config_path()))
    expect_false(file.exists(lam_dockerignore_path()))
  })
})

test_that("`write_config()` does the right thing", {
  with_local_project({
    unlink(lam_config_path()) # Otherwise `usethis::use_template()` doesn't write the new one
    cfg <- new_lambdar_config(list(app_name = "foo"))
    write_config(cfg)
    new_cfg <- yaml::read_yaml(lam_config_path())
    expect_equal(new_cfg$app_name, "foo")
  })
})

test_that("`write_dockerfile()` does the right thing", {
  with_local_project({
    cfg <- new_lambdar_config(list(lambda_handlers = "foo.bar"))
    write_dockerfile(cfg)
    df_lines <- readLines(lam_dockerfile_path())
    expect_true(any(grepl("foo.bar", df_lines, fixed = TRUE)))
  })
})
