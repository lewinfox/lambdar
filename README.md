# lambdar

<!-- badges: start -->
<!-- badges: end -->

The goal of `lambdar` is to provide a plug-and-play solution to running R on AWS lambda. The initial
code is a straight copy of [mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda).
Also see the 
[acompanying blog post](https://mdneuzerling.com/post/r-on-aws-lambda-with-containers/).


## Testing locally

First, make sure you have [Docker installed](https://docs.docker.com/get-docker/).

Create a new RStudio project. Mine is called `lambdar-test`.

Create a file called `main.R` containing a function called `hello_world().

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
Hello, World!
```

If your function accepts arguments, you can pass in a JSON payload:

``` bash
$ curl http://localhost/2015-03-31/functions/function/invocation -d '{"name": "R"}'
Hello, R!
```

## The plan

* Tag your function with `#' @lambda`
* Call `lambdar::use_lambdar()`
* It identifies the tagged function, checks there's only one
* Identifies dependencies (how? is this too hard?)
* Writes a `_lambdar.yml` file to the project root dir, which contains all the params needed
  * Main function identifier
  * Package list
  * Linux package list? If you need extra stuff?
  * Data files to be included in the container
* Once the `_lambdar.yml` file is ready, `lambdar` uses it to generate a `Dockerfile`
* Once the `Dockerfile` is ready, use `docker` to build a container.
* Tells you how to test the container locally.
* Helps you automate the process of uploading to AWS
* Provide a GitHub Actions template so we can update on every push etc.
