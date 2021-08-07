# lambdar

<!-- badges: start -->
<!-- badges: end -->

The goal of `lambdar` is to provide a plug-and-play solution to running R on AWS lambda. The initial
code is a straight copy of [mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda).
Also see the 
[acompanying blog post](https://mdneuzerling.com/post/r-on-aws-lambda-with-containers/).


## Testing locally

First, make sure you have [Docker installed](https://docs.docker.com/get-docker/).

Then, from the repo root:

Build the container. I'm tagging mine "`lewinfox/lambdar`".

``` bash
# Change into the Dockerfile location and build it.
cd inst/templates && docker build -t lewinfox/lambdar .
```

Run the container:

``` bash
# The first argument after the conatiner name is the `_HANDLER` environment variable expected by
# Lambda, in the format `filename.function_name`.
$ docker run -p 9000:8080 lewinfox/lambdar functions.hello_world
```

When the container runs it sets up a server that listens on
`http://localhost:8080/2015-03-31/functions/function/invocations`. Because we mapped port 9000 on our local
machine to port 8080 on the container in the previous step, we need to make requests to 
`http://localhost:9000/2015-03-31/functions/function/invocations`.

``` bash
# Passing no arguments
$ curl http://localhost:9000/2015-03-31/functions/function/invocations
Hello, World!

# Passing a JSON payload
$ curl http://localhost:9000/2015-03-31/functions/function/invocations -d '{"name": "R"}'
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
