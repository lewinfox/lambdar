# ---- Functions: AWS endpoints ----

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
#' These endpoints follow a set template and are defined by two variables, the environment variable
#' `AWS_LAMBDA_RUNTIME_API` (set at the session level) and the header
#' `Lambda-Runtime-Aws-Request-Id` which is different for every invocation of the lambda function.
#'
#' The generic constructor `aws_endpoint()` obtains the `LAMBDA_RUNTIME_API` variable for us (and
#' will throw an error if it is not present). The event handler code needs to obtain the
#' `Lambda-Runtime-Aws-Request-Id` header each time it's invoked.
#'
#' @param aws_request_id `Lambda-Runtime-Aws-Request-Id` header of the event
#' @param ... Path elements to be concatenated with `"/"`
#'
#' @name endpoints
NULL

#' @describeIn endpoints Generic endpoint constructor
aws_endpoint <- function(...) {
  # Check for the presence of the relevant env var so we can construct the endpoints
  lambda_runtime_api <- Sys.getenv("AWS_LAMBDA_RUNTIME_API")

  if (lambda_runtime_api == "") {
    error_message <- "'AWS_LAMBDA_RUNTIME_API' environment variable undefined"
    logger::log_error(error_message)
    signal_runtime_error(error_message)
  }

  base_url <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime")
  paste(base_url, ..., sep = "/")
}

#' @describein endpoints URL to query for the next event to process
aws_next_invocation_endpoint <- function() {
  aws_endpoint("invocation", "next")
}

#' @describeIn endpoints URL to return the result of the lambda function to
aws_response_endpoint <- function(aws_request_id) {
  aws_endpoint("invocation", aws_request_id, "response")
}

#' @describeIn endpoints URL to report to if we fail to intialise the runtime
aws_initialisation_error_endpoint <- function() {
  aws_endpoint("init", "error")
}

#' @describeIn endpoints URL to report invocation errors (i.e. lambda function errors) to
aws_invocation_error_endpoint <- function(aws_request_id) {
  aws_endpoint("invocation", aws_request_id, "error")
}


# ---- Functions: utils ----

#' Convert a list to a single character, preserving names
#' prettify_list(list("a" = 1, "b" = 2, "c" = 3))
#' # "a=5, b=5, c=5"
prettify_list <- function(x) {
  paste(
    paste(names(x), x, sep = "="),
    collapse = ", "
  )
}

# ---- Functions: error handling ----

#' Signal an API error
#'
#' This raises a custom error class `"lambdar_runtime_error"` to indicate that something has gone
#' wrong in our runtime. This is distinct from an error that occurs in the lambda function itself.
#'
#' @param message The error message to return
#' @param call The function call in which the error occurred.
#' @param ... Other data to be included in the error.
signal_runtime_error <- function(message, call = sys.call(-1), ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c("lambdar_runtime_error", "error", "condition")
  )
  logger::log_error(paste0("Lambdar runtime error: ", message))
  stop(err)
}

#' Signal an error in the lambda function
#'
#' This raises a custom error class `"lambdar_lambda_error"` to indicate that something has gone
#' wrong in our runtime. This is distinct from an error that occurs in the lambda function itself.
#'
#' @param message The error message to return
#' @param call The function call in which the error occurred.
#' @param ... Other data to be included in the error.
signal_lambda_error <- function(message, call = sys.call(-1), ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c("lambdar_lambda_error", "error", "condition")
  )
  logger::log_error(paste0("Lambdar function error: ", message))
  stop(err)
}


# ---- Functions: event handling ----

#' Handle an API event
#'
#' The runtime's main loop polls the "next invocation endpoint" repeatedly. When there is a new
#' task in the queue, the details of the task are returned to the runtime as an HTTP response. This
#' event needs to be handled.
#'
#' @param event The event object, an HTTP request.
handle_event <- function(event) {

  status_code <- httr::status_code(event)

  logger::log_trace("Event status code:", status_code)

  if (status_code != 200) {
    msg <- paste(
      "Received a bad response from the 'next invocation' endpoint.",
      "Status code:", status_code
    )
    signal_runtime_error(msg)
  }

  event_headers <- httr::headers(event)

  # HTTP headers are case-insensitive
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))

  aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
  if (is.null(aws_request_id)) {
    signal_runtime_error("Could not find 'lambda-runtime-aws-request-id' header in event", code = 400)
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

  # Record the event payload in the log
  if (!is.null(event_headers[["content-length"]])) {
    if (as.integer(event_headers[["content-length"]]) > 0) {
      logger::log_debug("Event payload:", unparsed_content)
    } else {
      logger::log_debug("Event payload: <none>")
    }
  }

  if (identical(unparsed_content, "") || is_scheduled_event) {
    # (1a) direct invocation with no args (or scheduled request)
    event_content <- list()
  } else {
    # (1b, 2a or 2b)
    event_content <- jsonlite::fromJSON(unparsed_content)
  }

  # If you want to do any additional inspection of the event body (including other http request
  # elements if it's an endpoint), you can do that here!

  # Change `http_req_element` if you'd prefer to send the http request `body` to the lambda fn,
  # rather than the query parameters (note that query string params are always strings! your lambda
  # fn may need to convert them back to numeric/logical/Date/etc.)
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

  # TODO: The error handling needs more work.
  #
  #       * How do we treat warnings?
  #       * What do we do with error codes / error statuses?
  #       * Really we should call the lambda in a new environment in case anyone starts messing
  #         about with the global env or other nonsense.
  #       * Are HTTP response codes the correct thing to be sending here? Seems like we risk
  #         confusing people because we're mixing custom "status" values with standard "status_code"
  #         values.
  response_object <- withCallingHandlers(
    {
      # Create a dummy result variable. This is so - in the event of a warning - we have an object
      # available to update.
      res <- list(
        result = NULL,
        status = "ok"
      )

      # To minimise the risk of lambda code modifying important bits of the runtime's environment
      # and potentially breaking stuff (what happens if it modifies environment variables like
      # AWS_LAMBDA_RUNTIME_API?) we will execute it in a fresh empty environment. This means that we
      # need to resolve the name to a function object before we use `do.call()`.
      lambda_function <- match.fun(function_name)
      lambda_execution_env <- new.env(parent = emptyenv())
      res$result <- do.call(lambda_function, event_content, envir = lambda_execution_env)
      res
    },
    warning = function(w) {
      # In case of warnings we want to still return the result of the function, but add details of
      # the warning/s that arose.
      #
      # TODO: This seems like a good candidate for an env var to determine whether we treat warnings
      #       as errors. It seems like we ought to by default. In this case maybe the appropriate
      #       and consistent thing to do is signal "there was an error in the lambda function" and
      #       let the main loop code handle it. However, we would need to work out if/how to pass
      #       the result up the chain.
      cnd_msg <- conditionMessage(w)
      cnd_call <- conditionCall(w)

      res$status <<- "warning"
      res$warning_messages <<- c(res$warning, cnd_msg) # If we hit multiple warnings, return all of them

      msg <- paste0("In '", cnd_call, "()': ", cnd_msg)
      logger::log_warn(msg)
    },
    error = function(e) {
      # Signal "the lambda function broke". This will be handled by the main loop's error handler
      # and the client will be notified.
      signal_lambda_error(conditionMessage(e), call = conditionCall(e))
    }
  )

  # AWS api gateway is a bit particular about the response format
  body <- if (is_http_req) {
    list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body =  as.character(jsonlite::toJSON(response_object, auto_unbox = TRUE))
    )
  } else {
    response_object
  }

  logger::log_debug("Response:", jsonlite::toJSON(body, auto_unbox = TRUE))

  # Send the response
  httr::POST(
    url = aws_response_endpoint(aws_request_id),
    body = body,
    encode = "json"
  )
}


# ---- Runtime: Set up logging ----

logger::log_formatter(logger::formatter_paste)

# Has the user supplied a logging level as an env var? If so, honour it unless we don't recognise it
# as a valid `logger` log level, in which case throw a runtime error.
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
    signal_runtime_error(paste0("Invalid log level '", log_level_env_var, "' provided"))
  )
  logger::log_threshold(log_level)
}

# ---- Runtime: Set up endpoints ----

next_invocation_endpoint <- aws_next_invocation_endpoint()
initialisation_error_endpoint <- aws_initialisation_error_endpoint()


# ---- Runtime: Set up runtime ----

# Run through the runtime setup process. If anything goes wrong, use `signal_runtime_error()` to
# signal to AWS that we weren't able to initialise the runtime correctly.
#
# The checks we perform are:
#
# 1. Make sure that we have a handler specified, and that we can identify the function and file.
tryCatch(
  {
    logger::log_debug("Determining handler from environment variables")

    handler <- Sys.getenv("_HANDLER")
    if (is.null(handler) || handler == "") {
      signal_runtime_error("_HANDLER environment variable undefined")
    }

    logger::log_info(paste0("Handler specified: '", handler, "'"))

    handler_split <- strsplit(handler, ".", fixed = TRUE)[[1L]]
    file_name <- paste0(handler_split[1L], ".R")
    function_name <- handler_split[2L]

    logger::log_debug(paste0("Checking if '", file_name, "' exists"))

    if (!file.exists(file_name)) {
      signal_runtime_error(paste(file_name, "not found in container dir", getwd()))
    }

    source(file_name)

    # Make sure that the handler function is defined in the file
    logger::log_debug(paste0("Checking if '", function_name, "()' is defined"))
    if (!exists(function_name)) {
      msg <- paste0("Function name '", function_name, "()' isn't defined in ", file_name)
      signal_runtime_error(msg)
    }

    logger::log_debug(paste0("Checking if '", function_name, "()' is a function"))

    if (!is.function(eval(parse(text = function_name)))) {
      msg <- paste0("'", function_name, "()' is not a function")
      signal_runtime_error(msg)
    }

    # Everything checks out, we can use this function
    logger::log_info(paste0("Using function '", function_name, "()' from '", file_name, "'"))
  },
  lambdar_runtime_error = function(e) {
    logger::log_trace("Handling a `lambdar_runtime_error` during runtime setup")
    # Something broke when we were trying to set up the API, so report this to the AWS
    # "initialisation error endpoint".
    logger::log_error(as.character(e))

    httr::POST(
      url = initialisation_error_endpoint,
      body = list(
        status = "runtime_error",
        error_message = as.character(e$message)),
      encode = "json"
    )
    stop(e)
  }
)

logger::log_info("Runtime setup successful")


# ---- Runtime: Main loop ----

repeat {

  tryCatch(
    {
      event <- httr::GET(url = next_invocation_endpoint)
      logger::log_info("New event received")
      handle_event(event)
    },
    lambdar_runtime_error = function(e) {
      # If this handler is triggered it means our runtime code has failed.
      logger::log_error(paste("Runtime error:", as.character(e)))

      # Extract headers
      headers <- httr::headers(event)
      names(headers) <- tolower(names(headers))
      aws_request_id <- headers[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {
        # We have the request ID so we can be good citizens and report failure to AWS
        logger::log_trace("POSTing invocation error for ID:", aws_request_id)

        httr::POST(
          url = aws_invocation_error_endpoint(aws_request_id),
          body = list(
            status = "runtime_error",
            error_message = as.character(e$message)
          ),
          encode = "json"
        )

      } else {
        # For whatever reason the request ID doesn't exist so we can't report failure
        logger::log_warn("No invocation ID! Can't clear this request from the queue.")
      }
    },
    lambdar_lambda_error = function(e) {
      # If this handler is triggered it means our lambda function has failed.
      logger::log_error("Lambda function error:", as.character(e))

      # Extract headers
      headers <- httr::headers(event)
      names(headers) <- tolower(names(headers))
      aws_request_id <- headers[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {
        # We have the request ID so we can be good citizens and report failure to AWS
        logger::log_trace("POSTing invocation error for ID:", aws_request_id)

        httr::POST(
          url = aws_invocation_error_endpoint(aws_request_id),
          body = list(
            status = "error",
            error_message = as.character(e$message)
          ),
          encode = "json"
        )
      } else {
        logger::log_warn("No invocation ID! Can't clear this request from the queue.")
      }
    },
    error = function(e) {
      # This is to catch other errors that we haven't thought of - probably a failure in obtaining
      # the next event

      logger::log_error("Unhandled error occurred while handling event:", as.character(e))

      # Extract headers
      headers <- httr::headers(event)
      names(headers) <- tolower(names(headers))
      aws_request_id <- headers[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {

        logger::log_trace("POSTing invocation error for ID:", aws_request_id)

        httr::POST(
          url = aws_invocation_error_endpoint(aws_request_id),
          body = list(
            status = "unknown_error",
            error_message = as.character(e)
          ),
          encode = "json"
        )
      } else {
        logger::log_warn("No invocation ID! Can't clear this request from the queue.")
      }
    }
  )
}
