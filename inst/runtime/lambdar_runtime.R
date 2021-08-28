# ---- Function definitions ----

#' Response endpoints
#'
#' @section Details:
#' The AWS lambda environment defines four HTTP endpoints that we need to communicate with in order
#' to receive new events, return results and report errors. These are:
#'
#' * `next_invocation_endpoint`: Our runtime should poll this endpoint continuously to check for new
#'   events. When an event is present we will receive it in the response.
#' * `response_endpoint`: Once we have the result of our lambda we should return th result to this
#'   endpoint so it can be passed on to our client.
#' * `initialisation_error_endpoint`: If something goes wrong initialising the runtime we should
#'   report it to this endpoint.
#' * `invocation_error_endpoint`: If something goes wrong with a particular lambda invocation, we
#'   should report it to this endpoint.
#'
#' @param aws_request_id Lambda-Runtime-Aws-Request-Id header of the event
#' @param ... Path elements to be concatenated with `"/"`
#'
aws_endpoint <- function(...) {
  # Check for the presence of the relevant env var so we can construct the endpoints
  lambda_runtime_api <- Sys.getenv("AWS_LAMBDA_RUNTIME_API")

  if (lambda_runtime_api == "") {
    error_message <- "'AWS_LAMBDA_RUNTIME_API' environment variable undefined"
    logger::log_error(error_message)
    stop(error_message) # TODO: Should this be `stop_api()`?
  }

  base_url <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime")
  paste(base_url, ..., sep = "/")
}

#' @describein aws_endpoint URL to query for the next event to process
aws_next_invocation_endpoint <- function() {
  aws_endpoint("invocation", "next")
}

#' @describeIn aws_endpoint URL to return the result of the lambda function to
aws_response_endpoint <- function(aws_request_id) {
  aws_endpoint("invocation", aws_request_id, "response")
}

#' @describeIn aws_endpoint URL to report to if we fail to intialise the runtime
aws_initialisation_error_endpoint <- function() {
  aws_endpoint("init", "error")
}

#' @describeIn aws_endpoint URL to report invocation errors (i.e. lambda function errors) to
aws_invocation_error_endpoint <- function(aws_request_id) {
  aws_endpoint("invocation", aws_request_id, "error")
}

#' Convert a list to a single character, preserving names
#' prettify_list(list("a" = 1, "b" = 2, "c" = 3))
#' # "a=5, b=5, c=5"
prettify_list <- function(x) {
  paste(
    paste(names(x), x, sep = "="),
    collapse = ", "
  )
}

#' Signal an API error
#'
#' This raises a custom error class `"lambdar_api_error"` to indicate that something has gone wrong
#' in our runtime.
#'
#' @param message The error message to return
#' @param code The response code to return. Defaults to 500 for "Internal Server Error". See
#'   [Server Error Responses on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status#server_error_responses)
#'   for other options.
#' @param call
#' @param ...
stop_api <- function(message, code = 500, call = sys.call(-1), ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      code = code,
      ...
    ),
    class = c("lambdar_api_error", "error", "condition")
  )
  logger::log_error(paste0("Lambdar API error (code ", code, "): ", message))
  stop(err)
}

#' Handle an API event
#'
#' The runtime's main loop polls the "next invocation endpoint" repeatedly. When there is a new
#' task in the queue, the details of the task are returned to the runtime as an HTTP response. This
#' event needs to be handled.
#'
#' @param event The event object, an HTTP request.
handle_event <- function(event) {

  status_code <- httr::status_code(event)

  logger::log_debug("New event received, status code:", status_code)

  if (status_code != 200) {
    msg <- paste(
      "Received a bad response from the 'next invocation' endpoint.",
      "Status code:", status_code
    )
    stop_api(msg, code = 400)
  }

  event_headers <- httr::headers(event)

  # HTTP headers are case-insensitive
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))

  aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
  if (is.null(aws_request_id)) {
    stop_api("Could not find 'lambda-runtime-aws-request-id' header in event", code = 400)
  }

  # According to the AWS guide, the below is used by "X-Ray SDK". All we need to do is set the env
  # var if the header is present.
  runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  # We need to parse the event in four contexts before sending to the lambda fn:
  #
  # 1a) Direct invocation with no function args (empty event)
  # 1b) Direct invocation with function args (parse and send entire event)
  # 2a) API endpoint with no args (parse HTTP request, confirm null request
  #     element; send empty list)
  # 2b) API endpoint with args (parse HTTP request, confirm non-null request
  #     element; extract and send it)

  unparsed_content <- httr::content(event, "text", encoding = "UTF-8")
  # Thank you to Menno Schellekens for this fix for Cloudwatch events
  is_scheduled_event <- grepl("Scheduled Event", unparsed_content)
  if (is_scheduled_event) {
    logger::log_info("Event type is 'scheduled'")
  }

  logger::log_debug("Unparsed content:", unparsed_content)

  if (identical(unparsed_content, "") || is_scheduled_event) {
    # (1a) direct invocation with no args (or scheduled request)
    event_content <- list()
  } else {
    # (1b, 2a or 2b)
    event_content <- jsonlite::fromJSON(unparsed_content)
  }

  # if you want to do any additional inspection of the event body (including
  # other http request elements if it's an endpoint), you can do that here!

  # change `http_req_element` if you'd prefer to send the http request `body` to
  # the lambda fn, rather than the query parameters
  # (note that query string params are always strings! your lambda fn may need to
  # convert them back to numeric/logical/Date/etc.)
  is_http_req <- FALSE
  http_req_element <- "queryStringParameters"

  if (http_req_element %in% names(event_content)) {
    is_http_req <- TRUE
    if (is.null(event_content[[http_req_element]])) {
      # (2a) api request with no args
      event_content <- list()
    } else {
      # (2b) api request with args
      event_content <- event_content[[http_req_element]]
    }
  }

  # TODO: Proper error handling. Think about what it should return on failures.
  response_object <- list(result = try(do.call(function_name, event_content), silent = TRUE))

  if (inherits(response_object$result, "try-error")) {
    response_object$result <- NULL # TODO: We would like to return some more info about the error here
    response_object$status <- "failure"
  } else {
    response_object$status <- "success"
  }

  logger::log_debug("Result:", as.character(response_object$result))

  response_endpoint <- aws_response_endpoint(aws_request_id)

  # aws api gateway is a bit particular about the response format
  body <- if (is_http_req) {
    list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body =  as.character(jsonlite::toJSON(response_object, auto_unbox = TRUE))
    )
  } else {
    response_object
  }

  httr::POST(
    url = response_endpoint,
    body = body,
    encode = "json"
  )
}


# ---- Set up logging----

logger::log_formatter(logger::formatter_paste)

# Has the user supplied a logging level as an env var? If so, honour it unless we don't recognise it
# as a valid `logger` log level, in which case throw an API error.
log_level_env_var <- toupper(Sys.getenv("LAMBDAR_LOG_LEVEL"))

if (identical(log_level_env_var, "")) {
  # Use INFO as the default
  logger::log_threshold(logger::INFO)
} else {
  # Check the input is valid
  log_level <- switch (
    log_level_env_var,
    "FATAL" = logger::FATAL,
    "ERROR" = logger::ERROR,
    "WARN"  = logger::WARN,
    "SUCCESS" = logger::SUCCESS,
    "INFO"  = logger::INFO,
    "DEBUG" = logger::DEBUG,
    "TRACE" = logger::TRACE,
    stop_api(paste("Invalid log level ", log_level_env_var, "provided"), code = 400)
  )
  logger::log_threshold(log_level)
}

# ---- Set up endpoints ----
next_invocation_endpoint <- aws_next_invocation_endpoint()
initialisation_error_endpoint <- aws_initialisation_error_endpoint()


# ---- Set up runtime ----

# Run through the runtime setup process. If anything goes wrong, use `stop_api()` to signal to
# AWS that we weren't able to initialise the runtime correctly.
#
# The checks we perform are:
#
# 1. Make sure that we have a handler specified, and that we can identify the function and file.
tryCatch(
  {
    logger::log_debug("Determining handler from environment variables")

    handler <- Sys.getenv("_HANDLER")
    if (is.null(handler) || handler == "") {
      stop_api("_HANDLER environment variable undefined")
    }

    logger::log_info("Handler found:", handler)

    handler_split <- strsplit(handler, ".", fixed = TRUE)[[1]]
    file_name <- paste0(handler_split[1], ".R")
    function_name <- handler_split[2]

    logger::log_info(paste0("Using function '", function_name, "()' from ", file_name))

    logger::log_debug("Checking if", file_name, "exists")

    if (!file.exists(file_name)) {
      stop_api(paste(file_name, "not found in container dir", getwd()))
    }

    source(file_name)

    # Make sure that the handler function is defined in the file
    logger::log_debug(paste0("Checking if '", function_name, "()' is defined"))
    if (!exists(function_name)) {
      msg <- paste0("Function name '", function_name, "()' isn't defined in ", file_name)
      stop_api(msg, code = 400)
    }

    logger::log_debug(paste0("Checking if '", function_name, "()' is a function"))

    if (!is.function(eval(parse(text = function_name)))) {
      msg <- paste0("'", function_name, "()' is not a function")
      stop_api(msg, code = 400)
    }
  },
  lambdar_api_error = function(e) {
    # Something broke when we were trying to set up the API, so report this to the AWS
    # "initialisation error endpoint".
    logger::log_error(as.character(e))

    httr::POST(
      url = initialisation_error_endpoint,
      body = list(
        statusCode = e$code,
        error_message = as.character(e$message)),
      encode = "json"
    )
    stop(e)
  }
)

logger::log_info("Runtime setup successful")
logger::log_info("Polling for events")


# ---- Main runtime loop ----

repeat {
  tryCatch(
    {
      event <- httr::GET(url = next_invocation_endpoint)
      logger::log_info("New event received")
      handle_event(event)
    },
    lambdar_api_error = function(e) {
      # If this handler is triggered it means our runtime code has failed.
      logger::log_error(paste("API error:", as.character(e)))

      # Extract headers
      headers <- httr::headers(event)
      names(headers) <- tolower(names(headers))
      aws_request_id <- headers[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {
        # We have the request ID so we can be good citizens and report failure to AWS
        logger::log_info("POSTing invocation error for ID:", aws_request_id)

        httr::POST(
          url = aws_invocation_error_endpoint(aws_request_id),
          body = list(
            statusCode = e$code,
            error_message = as.character(e$message)
          ),
          encode = "json"
        )

      } else {
        # For whatever reason the request ID doesn't exist so we can't report failure
        logger::log_warn("No invocation ID! Can't clear this request from the queue.")
      }
    },
    error = function(e) {

      logger::log_error(as.character(e))

      # Extract headers
      headers <- httr::headers(event)
      names(headers) <- tolower(names(headers))
      aws_request_id <- headers[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {

        logger::log_info("POSTing invocation error for ID:", aws_request_id)

        httr::POST(
          url = aws_invocation_error_endpoint(aws_request_id),
          body = list(error_message = as.character(e)),
          encode = "json"
        )
      } else {
        logger::log_warn("No invocation ID! Can't clear this request from the queue.")
      }
    }
  )
}
