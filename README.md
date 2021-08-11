# lambdar

<!-- badges: start -->
<!-- badges: end -->

The goal of `lambdar` is to provide a plug-and-play solution to running R on AWS lambda. The runtime
code (see `inst/runtime/lambdar_runtime.R`) is a straight copy of 
[mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda). The 
[acompanying blog post](https://mdneuzerling.com/post/r-on-aws-lambda-with-containers/) was helpful
when I was learning about Lambda runtimes.


## Setup

First, make sure you have [Docker installed](https://docs.docker.com/get-docker/).

Create a new RStudio project. Mine is called `lambdar-test` - lambdar will detect the name of your
project folder and use it to give a default name to your app.

In the root directory of your project, create a file called `main.R` containing the function you
want to access as a lambda. Use a roxygen-style `#' @lambda` tag to tell lambdar that this function should be lambda-fied. 

``` r
# main.R

#' @lambda
hello_world <- function(name = NULL) {
  if (is.null(name)) {
    name <- "World"
  }
  paste0("Hello, ", name, "!")
}
```

## Generate config file

Call `lambdar::use_lambdar()`. This will create two items in your project's root directory:

1. A `_lambdar.yml` file. This will be pre-populated with some metadata about your app. For this
   example you can leave it as-is.
2. A `.lambdar/` directory containing a single file, `lambdar_runtime.R`. This is the custom runtime
   that will be installed into the container to make everything work. Don't touch it!
   
``` r
lambdar::use_lambdar()
#> ✓ Setting active project to '/home/lewin/lambdar-test'
#> ✓ Creating /home/lewin/lambdar-test/.lambdar directory
#> ✓ Writing /home/lewin/lambdar-test/.lambdar/lambdar_runtime.R
#> ✓ Writing '_lambdar.yml'
#> • Modify '_lambdar.yml'
```

If you decide to lambda-fy a different function, call `lambdar::build_yaml()` to re-scan your code
for `@lambda` tags. You can always edit `_lambdar.yml` by hand as well - see (**TODO** - vignette).

## Build container

From here you can call `lambdar::build_dockerfile()` or `lambdar::build_container()`.
`build_container()` builds the Dockerfile as part of the process anyway, but if you want to review
the Dockerfile before building it, use `build_dockerfile()`. The Dockerfile will be created in your
project's root directory.

``` r
lambdar::build_dockerfile()
#> ✓ Writing 'Dockerfile'
#> ℹ To build your container, run `docker build -t lewin/lambdar-test .`
```

Or

``` r
lambdar::build_container()
#> ...
#> ... [lots of Docker output]
#> ...
#> ✓ Docker build successful
#> ℹ To start your container run `docker run -p 9000:8080 lewin/lambdar-test main.hello_world`
#> ℹ Once running you can send test queries to `http://localhost:9000/2015-03-31/functions/function/invocations`
```

## Test locally

The built container is based on an image supplied by AWS that includes a copy of something called
the Runtime Interface Emulator (RIE) that simulates the environment your function will run in. This
means you can work out the bugs in your function before deploying to AWS.

To start a container locally, use the Docker run command from the previous step.

``` bash
docker run -p 9000:8080 lewin/lambdar-test main.hello_world
```

In case you're not familiar with Docker syntax, this means "run the `lewin/lambdar-test` container,
redirecting traffic from local port 9000 to container port 8080". `"main.hello_world"` is a required
parameter that tells the Lambda runtime which handler we want this container to use. In this case
we're using the `hello_world()` function from the `main.R` file.

You can now query the demo endpoint using the tool of your choice to test your API.

``` bash
$ curl http://localhost:9000/2015-03-31/functions/function/invocation
{"result": "Hello, World!", "status": "success"}
```

If your function accepts arguments, you can pass in a JSON payload:

``` bash
$ curl http://localhost:9000/2015-03-31/functions/function/invocation -d '{"name": "R"}'
{"result": "Hello, R!", "status": "success"}
```

To stop your container use `docker stop`. If you have no other containers running you can use

``` bash
docker stop $(docker ps -q)
```

## Multiple endpoints

You are welcome to tag multiple functions with `@lambda`. The relevant source file/s will be copied
into the container, and you can launch a container for each handler in your `docker run` command.

If you want to have multiple containers running at once, remember to map them to different ports.

``` r
# main.R

#' @lambda
#' @lambda
hello_world <- function(name = NULL) {
  if (is.null(name)) {
    name <- "World"
  }
  paste0("Hello, ", name, "!")
}

#' @lambda
add_one <- function(x = NULL) {
  if (is.null(x)) {
    x <- 0
  } else {
    x <- as.numeric(x) 
  }
  x + 1
}
```

Call `lambdar::build_yaml()` to regenerate the config file. You'll notice that the `lambda_handlers`
entry now has two handlers:

``` yaml
# _lambdar.yml

# ...

lambda_handlers: [ "main.hello_world", "main.add_one" ]

# ...
```

Either of these can now be specified as a handler for a given container.

``` r
lambdar::build_container()
#> ... [ Docker output ] ...
#> ✓ Docker build successful
#> ℹ To start your container run `docker run -p 9000:8080 lewin/lambdar-test <handler>`
#> ℹ Possible values of `<handler>` are `main.hello_world` and `main.add_one`
#> ℹ Once running you can send test queries to `http://localhost:9000/2015-03-31/functions/function/invocations`
```

In two different terminal windows, run

``` bash
# One container listens on port 9000
$ docker run -p 9000:8080 lewin/lambdar-test main.hello_world

# The other on port 9001
$ docker run -p 9001:8080 lewin/lambdar-test main.add_one
```

You can now send queries to `http://localhost:9000/2015-03-31/functions/function/invocations` (for
the `hello_world()` function) and `http://localhost:9001/2015-03-31/functions/function/invocations`
(for the `add_one()` function).


## Including extra stuff in the container

You can tell lambdar to include extra files or packages in your container by editing `_lambdar.yml`.

### `include_files`

If your lambda depends on additional data or code files you need to add them to the`include_files`
array. By default only files containing `@lambdar`-tagged functions will be included.

``` yaml
# _lambdar.yml

include_files: [ "main.R", "data.csv", "utilities.R" ]
```

### `r_packages`

Any packages listed here will be installed when the container is built. **TODO**: Auto-populate this.

``` yaml
# _lambdar.yml

r_packages: [ tidyverse, units ]
```

### `linux_packages`

If you need any extra system libraries installing, mention them here. The base AWS image is based on
CentOS, so packages will be installed by `yum`.

``` yaml
# _lambdar.yml

linux_packages: [ udunits2-devel ]
```

## TODO - future work

* Identify and auto-populate dependencies (how?)
* Help you automate the process of uploading to AWS
* Provide a GitHub Actions template so we can update on every push etc.
* Duplicate the same functionality but using lambda layers (the other custom runtime option). See
  [medium.com/bakdata/running-r-on-aws-lambda](https://medium.com/bakdata/running-r-on-aws-lambda-9d40643551a6)
