# Pinched from https://github.com/jimhester/usethis/blob/de8aa116820a8e54f2f952b341039985d78d0352/tests/testthat/helper.R#L28-L68
# See https://github.com/r-lib/devtools/blob/78b4cabcba47e328e255fffa39e9edcad302f8a0/tests/testthat/test-build-readme.R
# for an example of how to use.

create_local_thing <- function(dir = fs::file_temp(),
                               env = parent.frame(),
                               rstudio = FALSE,
                               thing = c("package", "project")) {

  if (!requireNamespace("fs", quietly = TRUE)) {
    rlang::abort("Package `fs` is required but not installed.")
  }

  thing <- match.arg(thing)
  if (dir.exists(dir)) {
    usethis::ui_stop("Target {ui_code('dir')} {usethis::ui_path(dir)} already exists.")
  }


  old_project <- usethis::proj_get() # this could be `NULL`, i.e. no active project
  old_wd <- getwd()          # not necessarily same as `old_project`


  withr::defer(
    {
      usethis::ui_done("Deleting temporary project: {usethis::ui_path(dir)}")
      fs::dir_delete(dir)
    },
    envir = env
  )
  usethis::ui_silence(
    switch(
      thing,
      package = usethis::create_package(dir, rstudio = rstudio, open = FALSE, check_name = FALSE),
      project = usethis::create_project(dir, rstudio = rstudio, open = FALSE)
    )
  )


  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env)
  usethis::proj_set(dir)


  withr::defer(
    {
      usethis::ui_done("Restoring original working directory: {usethis::ui_path(old_wd)}")
      setwd(old_wd)
    },
    envir = env
  )
  setwd(usethis::proj_get())


  invisible(usethis::proj_get())
}
