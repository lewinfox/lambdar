# lambdar

<!-- badges: start -->
<!-- badges: end -->

The goal of `lambdar` is to provide a plug-and-play solution to running R on AWS lambda. The runtime
code (see `inst/runtime/lambdar_runtime.R) is a straight copy of 
[mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda). The 
[acompanying blog post](https://mdneuzerling.com/post/r-on-aws-lambda-with-containers/) was helpful
when I was learning about Lambda runtimes.


## Quickstart guide

First, make sure you have [Docker installed](https://docs.docker.com/get-docker/).

Create a new RStudio project. Mine is called `lambdar-test` - lambdar will detect the name of your
project folder and use it to give a default name to your app.

In the root directory of your project, create a file called `main.R` containing the function you
want to access as a lambda.

``` r
# main.R
hello_world <- function(name = NULL) {
  if (is.null(name)) {
    name <- "World"
  }
  paste0("Hello, ", name, "!")
}
```

Call `lambdar::use_lambdar()`. This will create two items in your project's root directory:

1. A `_lambdar.yml` file. This will be pre-populated with some metadata about your app. For this
   example you can leave it as-is.
2. A `lambdar/` directory containing a single file, `lambdar_runtime.R`. This is the custom runtime
   that will be installed into the container to make everything work. Don't touch it!
   
``` r
lambdar::use_lambdar()
#> ✓ Setting active project to '/home/lewin/lambdar-test'
#> ✓ Creating /home/lewin/lambdar-test/lambdar directory
#> ✓ Writing /home/lewin/lambdar-test/lambdar/lambdar_runtime.R
#> ✓ Writing '_lambdar.yml'
#> • Modify '_lambdar.yml'
```
   
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
#> ℹ API endpoint: `http://localhost:9000/2015-03-31/functions/function/invocations`
```

You can now query that endpoint using the tool of your choice to test your API.

``` bash
$ curl http://localhost/2015-03-31/functions/function/invocation
{"result": "Hello, World!", "status": "success"}
```

If your function accepts arguments, you can pass in a JSON payload:

``` bash
$ curl http://localhost/2015-03-31/functions/function/invocation -d '{"name": "R"}'
{"result": "Hello, R!", "status": "success"}
```

To stop your container use `docker stop`. If you have no other containers running you can use
``` bash
docker stop $(docker ps -q)
```

## TODO - future work

* Tag your function with `#' @lambda`
* Call `lambdar::use_lambdar()`
* It identifies the tagged function, checks there's only one
* Identifies dependencies (how? is this too hard?)
* Writes a `_lambdar.yml` file to the project root dir, which contains all the params needed
  * Main function identifier
  * Package list
  * Linux package list, if needed
  * Data files to be included in the container
* Once the `_lambdar.yml` file is ready, `lambdar` uses it to generate a `Dockerfile`
* Once the `Dockerfile` is ready, use `docker` to build a container.
* Tells you how to test the container locally.
* Helps you automate the process of uploading to AWS
* Provide a GitHub Actions template so we can update on every push etc.
