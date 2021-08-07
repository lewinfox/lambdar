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

## Future work

Use a similar API to [plumber](https://www.rplumber.io/) to define a function (or functions?) to be 
lambda-d:

``` r
#' @lambda
hello <- function(name = NULL) {
  if (is.null(name)) {
    name <- "World"
  }
  paste("Hello," name) # This will need to be JSON-ified
}
```

Do some kind of compilation and upload step:

``` r
upload_functions_to_lambda()
#> Reading config
#> Doing security stuff
#> Zipping code
#> Uploading to lambda
#> Done - send requests to https://my-lambda-endpoint.com/whatever
```

Make a request:

```
curl -vX POST https://my-lambda-endpoint.com/whatever -H "Content-Type: application/json" -d '{"name": "R"}'
```

```
'{"response": "Hello, R"}'
```

Something like that, anyway. It would be even better to build on plumber and just get that working
in Lambda. Can you have sub-routes within a Lambda call? Yes, see 
[here](https://aws.amazon.com/blogs/compute/using-multiple-segments-in-amazon-api-gateway-base-path-mapping/).
