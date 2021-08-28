# lambdar <a href='https://lewinfox.github.io/lambdar'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of lambdar is to make it easy to run R on AWS lambda. AWS doesn't provide support for R
out of the box, but _does_ allow you to provide a custom runtime in the form of a container image or
lambda layers. Lambdar provides:

* An R runtime (thanks to [mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda))
* Tools to build, test and deploy Docker containers containing your code

The process is simple. You write your code as normal, decorate your lambda function with a
roxygen-style `@lambda` tag, and lambdar does the rest. It's designed to drop on top of your
existing work with almost no changes to your code.


## Installation

You can install the development version from GitHub with

``` r
devtools::install_github("lewinfox/lambdar")
```

[Docker](https://docs.docker.com/get-docker/) and the [AWS cli](https://aws.amazon.com/cli/) are 
recommended - without these lambdar can build Dockerfiles but can't create or deploy the actual 
image.


## Setup

Lambdar is designed to work in [R projects](https://r4ds.had.co.nz/workflow-projects.html). For this
example I've create a project in a directory called `lambdar-test`. The project contains one file,
`main.R`, which contains one function, `hello_world()`.



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

Call `lambdar::init()`. This will create the following in your project's root directory:

1. A `_lambdar.yml` file. This will be pre-populated with some metadata about your app. For this
   example you can leave it as-is.
2. A `.lambdar/` directory containing a single file, `lambdar_runtime.R`. This is the custom runtime
   that will be installed into the container to make everything work. Don't touch it!
3. A `.dockerignore` file, which lists the files and folders that should _not_ be copied into the
   final container image.
   
Lambdar also scans all the R files in your project looking for:

* Functions with `@lambda` tags (referred to as "handler functions")s
* R package dependencies

and writes this information int `_lambdar.yml`.

``` r
lambdar::init()
#> ✔ Setting active project to '/home/lewin/lambdar-test'
#> ✔ Creating .lambdar directory
#> ✔ Writing .lambdar/lambdar_runtime.R
#> ✔ Writing '.dockerignore'
#> ℹ Scanning project...
#> ✔ Found 1 handler function
#> ✔ Writing '_lambdar.yml'
```

If you change your code (for example, adding new package dependencies or changing which function is
tagged with `@lambda`), call `lambdar::build_config()` to re-scan your code and update 
`_lambdar.yml`.

## Edit `_lambdar.yml`

If you have an AWS account, add your AWS account ID and region to `_lambdar.yml`.

``` yaml
# _lambdar.yml

# Your 12-digit AWS ID
aws_account_id: "123456789012"

# Your preferred AWS region e.g. "ap-southeast-2"
aws_region: "ap-southeast-2"
```

For more information on the different configuration options see `vignette("configuration")`.


## Build the image

From here you can call `lambdar::build_dockerfile()` or `lambdar::build_image()`. `build_image()` 
builds the Dockerfile as part of the process anyway, but if you want to review the Dockerfile before
building it, use `build_dockerfile()`. The Dockerfile will be created in your project's root 
directory.

``` r
lambdar::build_dockerfile()
#> ✔ Setting active project to '/home/lewin/lambdar-test'
#> ✔ Writing 'Dockerfile'
#> ℹ To build your container, run `docker build -t lambdar-test .` or `lambdar::build_image()`
```

Or

``` r
lambdar::build_image()
#> → `docker build -t lambdar-test .`
#>
#> ... [lots of Docker output] ...
#>
#> ✓ Docker build successful
#> ℹ To start your container run `docker run -p 9000:8080 lambdar-test main.hello_world`
#> ℹ Once running you can send test queries to `http://localhost:9000/2015-03-31/functions/function/invocations`
```

## Test locally

The built container is based on an image supplied by AWS that includes a copy of something called
the Runtime Interface Emulator (RIE) that simulates the environment your function will run in. This
means you can work out the bugs in your function before deploying to AWS.

To start a container locally, use the Docker run command from the previous step.

``` bash
docker run -p 9000:8080 lambdar-test main.hello_world
```

In case you're not familiar with Docker syntax, this means "run the `lambdar-test` container,
redirecting traffic from local port 9000 to container port 8080". `"main.hello_world"` is a required
parameter that tells the Lambda runtime which handler we want this container to use. In this case
we're using the `hello_world()` function from the `main.R` file.

You can now query the demo endpoint using the tool of your choice to test your API.

``` bash
$ curl http://localhost:9000/2015-03-31/functions/function/invocations
{"result": "Hello, World!", "status": "success"}
```

If your function accepts arguments, you can pass in a JSON payload:

``` bash
$ curl http://localhost:9000/2015-03-31/functions/function/invocations -d '{"name": "R"}'
{"result": "Hello, R!", "status": "success"}
```

**NOTE**: All parameters from the JSON payload are passed to your function as text. If your function
expects any other input it needs to perform the conversion itself. For example:

``` R
# main.R

#' @lambda
add_one <- function(x = 0) {
  x <- as.numeric(x) # You should also add some error handling in case this fails
  x + 1
} 
```

To stop your container use `docker stop`. If you have no other containers running you can use

``` bash
docker stop $(docker ps -q)
```

## Upload to AWS

Once you've tested your function it's time to upload it to the AWS Elastic Container Registry. You 
can do this by hand following
[this guide](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/), or you 
can use `lambdar::upload_to_ecr()`.

``` R
lambdar::upload_to_ecr()
#> ✔ Setting active project to '/home/lewin/lambdar-test'
#> → `docker tag lambdar-test 123456789012.dkr.ecr.ap-southeast-2.amazonaws.com/lambdar-test:latest`
#> ✔ Suggessfully tagged `lambdar-test` as `123456789012.dkr.ecr.ap-southeast-2.amazonaws.com/lambdar-test:latest`
#> ℹ Creating the repository if it doesn't already exist
#> → `aws ecr create-repository --repository-name lambdar-test`
#> ℹ Authenticating Docker with AWS ECR
#> → `aws ecr get-login-password | docker login --username AWS --password-stdin 123456789012.dkr.ecr.ap-southeast-2.amazonaws.com`
#> ℹ Uploading container image
#> → `docker push 123456789012.dkr.ecr.ap-southeast-2.amazonaws.com/lambdar-test:latest`
#>
#> ... [lots of output] ...
#>
#> ✔ Successfully uploaded 123456789012.dkr.ecr.ap-southeast-2.amazonaws.com/lambdar-test:latest
```


## Create your lambda function

Create your lambda function from your newly-uploaded container image following the steps
[here](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/).


## TODO - future work

* Add a function to create the lambda for you.
* Provide a GitHub Actions template so we can update on every push etc.
* Duplicate the same functionality but using lambda layers (the other custom runtime option). See
  [medium.com/bakdata/running-r-on-aws-lambda](https://medium.com/bakdata/running-r-on-aws-lambda-9d40643551a6)
a
