# lambdar

<!-- badges: start -->
<!-- badges: end -->

The goal of `lambdar` is to provide a plug-and-play solution to running R on AWS lambda. The initial
inspiration was provided by [mdneuzerling/r-on-lambda](https://github.com/mdneuzerling/r-on-lambda).
Also see the 
[acompanying blog post](https://mdneuzerling.com/post/r-on-aws-lambda-with-containers/).


## The plan

Use a similar API to [plumber]() to define a function or functions to be lambda-d:

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
