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

  withr::with_options(list(usethis.quiet = TRUE), {
    old_project <- usethis::proj_get() # this could be `NULL`, i.e. no active project
    old_wd <- getwd()          # not necessarily same as `old_project`


    withr::defer(
      {
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

    withr::defer(usethis::ui_silence(usethis::proj_set(old_project, force = TRUE)), envir = env)
    usethis::proj_set(dir)

    withr::defer(
      {
        setwd(old_wd)
      },
      envir = env
    )

    setwd(usethis::ui_silence(usethis::proj_get()))
    invisible(usethis::ui_silence(usethis::proj_get()))
  })
}

# Create a local lambdar probect with config, runtime and a main.R file containing two handlers
with_local_project <- function(code) {
  withr::with_options(list(lambdar.quiet = TRUE, usethis.quiet = TRUE), {
    create_local_thing(thing = "project")
    init()
    main_text <- "
    #' @lambda
    hello_world <- function(name = NULL) {
      if (is.null(name)) {
        name <- 'World'
      }
      paste0('Hello, ' name, '!')
    }

    #' @lambda
    add_one <- function(x = 0) {
      x <- as.numeric(x)
      x + 1
    }
    "
    cat(main_text, file = "main.R")
    force(code)
  })
}
