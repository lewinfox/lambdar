# ---- Utility functions ----

# Custom condition for API errors (https://adv-r.hadley.nz/conditions.html)
condition <- function(subclass, message, code, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}

# Stopping function for internal API errors
stop_api <- function(message, code = 500, call = sys.call(-1), ...) {
  # TODO: Add the AWS_REQUEST_ID in here so we can handle it in the main loop code
  stop(condition(c("api_error", "error"), message = message, code = code, call = call), ...)
}

# Given a HANDLER like `foo.bar`, interpret it as the function `foo()` from the file `bar.R`
parse_handler <- function(handler) {
  x <- strsplit(handler, ".", fixed = TRUE)[[1]]
  list(file = paste0(x[1], ".R"), FUN = x[2])
}

# ---- Environment setup ----

# AWS Lambda requires us to make use of a few environment variables
AWS_LAMBDA_RUNTIME_API = Sys.getenv("AWS_LAMBDA_RUNTIME_API")
HANDLER <- Sys.getenv("_HANDLER")

# Check the vars
if (AWS_LAMBDA_RUNTIME_API == "") {
  stop("`AWS_LAMBDA_RUNTIME_API` environment variable not found")
}

if (HANDLER == "") {
  stop("`HANDLER` environment variable not found")
}

# Where should we check for the next payload, or report errors?
NEXT_INVOCATION_ENDPOINT <- paste0("http://", AWS_LAMBDA_RUNTIME_API, "/2018-06-01/runtime/invocation/next")
INITIALISATION_ERROR_ENDPOINT <- paste0("http://", AWS_LAMBDA_RUNTIME_API, "/2018-06-01/runtime/init/error")

# Make sure that we have everything we need. If not, notify Lambda using the
# `INITIALISATION_ERROR_ENDPOINT`
tryCatch(
  {
    # HANDLER is in the format `file.function`
    parsed_handler <- parse_handler(HANDLER)
    file <- parsed_handler[["file"]]
    FUN <- parsed_handler[["FUN"]]
    message("`HANDLER`: ", HANDLER)
    message("File: ", file, " Function: ", FUN)

    # Make sure we can find the file and function
    if (!file.exists(file)) {
      stop_api(paste("File", file, "not found"))
    }
    source(file)
    if (!exists(FUN)) {
      stop_api(paste("Function", FUN, "not found"))
    }
    # make sure that `FUN` is a function not something else
    if (!is.function(eval(parse(text = FUN)))) {
      stop_api(paste(FUN, "is not a function"))
    }
  },
  api_error = function(e) {
    # Something broke so we need to send details of the error to the `INITIALISATION_ERROR_ENDPOINT`
    httr::POST(
      url = INITIALISATION_ERROR_ENDPOINT,
      body = list(
        statusCode = e$code,
        error_message = as.character(e$message)
      ),
      encode = "json"
    )
    # Now that Lambda has been notified, stop
    stop(e)
  }
)

# ---- Event handler function ----

#' Handle a lambda event
#'
#' Take an input event, validate and parse it, pass it to the handler lambda function, and return
#' the result to the endpoint specified by AWS Lambda.
#'
#' @param event The HTTP event received from the "next invocation endpoint". Basically some form of
#'   HTTP request that may contain some data as JSON or query parameters.
#' @param lambda_function Either a function object or a string representing a function name.
#' @param AWS_LAMBDA_RUNTIME_API String. The value of the `AWS_LAMBDA_RUNTIME_API` environment
#'   variable.
handle_event <- function(event, lambda_function = FUN, AWS_LAMBDA_RUNTIME_API = Sys.getenv("AWS_LAMBDA_RUNTIME_API")) {
  # Check that this is a good event
  stat_code <- httr::status_code(event)
  if (stat_code != 200) {
    # Return 400 (Bad Request)
    stop_api(paste("Status code ", stat_code, "received - stopping execution and sending status code 400 (Bad Request)"), code = 400)
  }

  # HTTP headers are case-insensitive
  headers <- httr::headers(event)

  # The `Lambda-Runtime-Aws-Request-Id` header is needed to link requests/responses.
  # `Lambda-Runtime-Trace-Id` is something to do with the X-ray SDK, whatever that is.
  AWS_REQUEST_ID <- headers[["Lambda-Runtime-Aws-Request-Id"]]
  AWS_RUNTIME_TRACE_ID <- headers[["Lambda-Runtime-Trace-Id"]]

  if (is.null(AWS_REQUEST_ID)) {
    stop_api("Could not find `Lambda-Runtime-Aws-Request-Id` header in lambda event", code = 400)
  }

  if (is.null(AWS_RUNTIME_TRACE_ID)) {
    stop_api("Could not find `Lambda-Runtime-Trace-Id` header in lambda event", code = 400)
  }

  # If this trace id header is present, set it as an env var. We don't need to do anything further
  # with it.
  if (!is.null(AWS_RUNTIME_TRACE_ID)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = AWS_RUNTIME_TRACE_ID)
  }

  # Now we can construct the endpoint we will need to send our reply to
  AWS_RESPONSE_ENDPOINT <- paste0("http://", AWS_LAMBDA_RUNTIME_API, "/2018-06-01/runtime/invocation/", AWS_REQUEST_ID, "/response")

  # So, we have a good request and we've obtained the headers we need. Now we can start actually
  # passing the request data onto the handler.

  # We need to handle four possible scenarios before we pass the data on to the lambda function:
  #
  # 1a) direct invocation with no function args (empty event)
  # 1b) direct invocation with function args (parse and send entire event)
  # 2a) api endpoint with no args (parse HTTP request, confirm null request
  #     element; send empty list)
  # 2b) api endpoint with args (parse HTTP request, confirm non-null request
  #     element; extract and send it)

  # Extract the event contents
  raw_event_content <- httr::content(event, as = "text", encoding = "UTF-8")

  # This is apparently important for Cloudwatch events?
  is_scheduled_event <- grepl("Scheduled Event", raw_event_content)
  if (is_scheduled_event) {
    message("This is a scheduled event")
  }

  # Display the raw event content
  message("Event content: ", raw_event_content)

  # Default event_content is nothing (an empty list, i.e. an empty JSON)
  event_content <- list()

  # Check if the payload is empty, or it's a scheduled event
  if (raw_event_content == "" || is_scheduled_event) {
    # Do nothing, `event_content` is already an empty list
  } else {
    # There is some stuff here, convert it from JSON
    event_content <- jsonlite::fromJSON(raw_event_content)
    # TODO: Handle malformed JSON or does AWS deal with that?
  }

  # At this point we could do some more handling of the request, inspecting payload etc.

  # Now we can call the lambda! Note that the payload will always come in the form of strings, so
  # the onus is on the lambda to perform conversion to whatever datatypes it expects.

  # Work out whether we have received a HTTP request with query string parameters, or whether it's
  # a JSON.
  is_http_request <- FALSE
  http_request_element <- "queryStringParameters"

  if (http_request_element %in% names(event_content)) {
    is_http_request <- TRUE
    if (is.null(event_content[[http_request_element]])) {
      # 2a: API request with no args
      # Nothing needs doing because `event_content` is already a list.
    } else {
      # 2b: API request with args
      event_content <- event_content[[http_request_element]] # TODO: What does this look like?
    }
  }

  # The money shot! Do what we are here to do
  # TODO: How do we handle errors within the lambda function itself?
  res <- do.call(lambda_function, event_content)

  message("Result: ", res)

  # Now we have a result, package it up and send it back. First we build the response object:
  response_body <- NULL

  if (is_http_request) {
    response_body <- list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body = as.character(jsonlite::toJSON(res, auto_unbox = TRUE))
    )
  } else {
    response_body <- res
  }

  # Then we POST it back to the endpoint we constructed earlier
  httr::POST(
    url = AWS_RESPONSE_ENDPOINT,
    body = response_body,
    encode = "json"
  )
}

# ---- Main runtime loop ----

# Keep pinging the relevant endpoint. If you get a response, handle it using the function above.

repeat {
  tryCatch(
    {
      event <- httr::GET(url = NEXT_INVOCATION_ENDPOINT)
      handle_event(event)
    },
    api_error = function(e) {
      # Something broke in our API

      headers <- httr::headers(event)
      # TODO: Add this into the condition object so we don't need to fuck about extracting it here
      AWS_REQUEST_ID <- headers[["Lambda-Runtime-Aws-Request_Id"]]

      if (!is.null(AWS_REQUEST_ID)) {
        message("Error processing request id ", AWS_REQUEST_ID)

        # Reply to the error reporting endpoint saying something went wrong
        INVOCATION_ERROR_ENDPOINT <- paste0("http://", AWS_LAMBDA_RUNTIME_API, "2018-06-01/runtime/invocation/", AWS_REQUEST_ID, "/error")
        httr::POST(
          url = INVOCATION_ERROR_ENDPOINT,
          body = list(
            statusCode = e$code,
            error_message = as.character(e$message)
          ),
          encode = "json"
        )
      } else {
        # Something went wrong and we don't have a request ID to reply with
        warning("Error processing request - no invocation ID so cannot clear this request from the queue")
      }
    },
    error = function(e) {
      # These are general R errors, not errors generated from our handler
      warning("R runtime error: ", e)

      # As before, extract the necessary info from the headers and let Lambda know about the error
      headers <- httr::headers(event)

      AWS_REQUEST_ID <- headers[["Lambda-Runtime-Aws-Request_Id"]]

      if (!is.null(AWS_REQUEST_ID)) {
        INVOCATION_ERROR_ENDPOINT <- paste0("http://", AWS_LAMBDA_RUNTIME_API, "2018-06-01/runtime/invocation/", AWS_REQUEST_ID, "/error")
        httr::POST(
          url = INVOCATION_ERROR_ENDPOINT,
          body = list(
            statusCode = e$code,
            error_message = as.character(e$message)
          ),
          encode = "json"
        )
      } else {
        warning("R runtime error: ", e, ". No request ID so cannot clear this event from the queue")
      }
    }
  )
}
