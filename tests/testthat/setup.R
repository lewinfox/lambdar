# Pinched from https://github.com/jimhester/usethis/blob/de8aa116820a8e54f2f952b341039985d78d0352/tests/testthat/helper.R#L28-L68
# See https://github.com/r-lib/devtools/blob/78b4cabcba47e328e255fffa39e9edcad302f8a0/tests/testthat/test-build-readme.R
# for an example of how to use.

create_local_thing <- function(dir = file_temp(pattern = pattern),
                               env = parent.frame(),
                               rstudio = FALSE,
                               thing = c("package", "project")) {
  thing <- match.arg(thing)
  if (fs::dir_exists(dir)) {
    ui_stop("Target {ui_code('dir')} {ui_path(dir)} already exists.")
  }
  
  
  old_project <- proj_get_() # this could be `NULL`, i.e. no active project
  old_wd <- getwd()          # not necessarily same as `old_project`
  
  
  withr::defer(
    {
      ui_done("Deleting temporary project: {ui_path(dir)}")
      fs::dir_delete(dir)
    },
    envir = env
  )
  ui_silence(
    switch(
      thing,
      package = create_package(dir, rstudio = rstudio, open = FALSE, check_name = FALSE),
      project = create_project(dir, rstudio = rstudio, open = FALSE)
    )
  )
  
  
  withr::defer(proj_set(old_project, force = TRUE), envir = env)
  proj_set(dir)
  
  
  withr::defer(
    {
      ui_done("Restoring original working directory: {ui_path(old_wd)}")
      setwd(old_wd)
    },
    envir = env
  )
  setwd(proj_get())
  
  
  invisible(proj_get())
}