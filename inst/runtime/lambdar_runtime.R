logger::log_formatter(logger::formatter_paste)
logger::log_threshold(logger::INFO)

#' Convert a list to a single character, preserving names
#' prettify_list(list("a" = 1, "b" = 2, "c" = 3))
#' # "a=5, b=5, c=5"
prettify_list <- function(x) {
  paste(
    paste(names(x), x, sep = "="),
    collapse = ", "
  )
}

# error handling with http codes
# from http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, code, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}

#' Signal an API error
stop_api <- function(message, code = 500, call = sys.call(-1), ...) {
  stop(condition(c("api_error", "error"), message, code = code, call = call, ...))
}

logger::log_debug("Deriving lambda runtime API endpoints from environment variables")

lambda_runtime_api <- Sys.getenv("AWS_LAMBDA_RUNTIME_API")
if (lambda_runtime_api == "") {
  error_message <- "AWS_LAMBDA_RUNTIME_API environment variable undefined"
  logger::log_error(error_message)
  stop(error_message)
}

next_invocation_endpoint <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next")

initialisation_error_endpoint <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime/init/error")

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

    logger::log_info("Using function", function_name, "from", file_name)

    logger::log_debug("Checking if", file_name, "exists")

    if (!file.exists(file_name)) {
      stop_api(file_name, " doesn't exist in ", getwd())
    }

    source(file_name)

    logger::log_debug("Checking if", function_name, "is defined")
    if (!exists(function_name)) {
      stop_api("Function name ", function_name, " isn't defined in R")
    }

    logger::log_debug("Checking if", function_name, "is a function")

    if (!is.function(eval(parse(text = function_name)))) {
      stop_api("Function name ", function_name, " is not a function")
    }
  },
  api_error = function(e) {
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

handle_event <- function(event) {
  status_code <- httr::status_code(event)
  logger::log_debug("Status code:", status_code)
  if (status_code != 200) {
    stop_api("Didn't get status code 200. Status code: ", status_code, code = 400)
  }

  event_headers <- httr::headers(event)

  # HTTP headers are case-insensitive
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))

  aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
  if (is.null(aws_request_id)) {
    stop_api("Could not find lambda-runtime-aws-request-id header in event", code = 400)
  }

  # According to the AWS guide, the below is used by "X-Ray SDK"
  runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  # we need to parse the event in four contexts before sending to the lambda fn:
  # 1a) direct invocation with no function args (empty event)
  # 1b) direct invocation with function args (parse and send entire event)
  # 2a) api endpoint with no args (parse HTTP request, confirm null request
  #   element; send empty list)
  # 2b) api endpoint with args (parse HTTP request, confirm non-null request
  #   element; extract and send it)

  unparsed_content <- httr::content(event, "text", encoding = "UTF-8")
  # Thank you to Menno Schellekens for this fix for Cloudwatch events
  is_scheduled_event <- grepl("Scheduled Event", unparsed_content)
  if (is_scheduled_event) {
    logger::log_info("Event type is scheduled")
  }

  logger::log_debug("Unparsed content:", unparsed_content)

  if (unparsed_content == "" || is_scheduled_event) {
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

  response_object <- list(result = try(do.call(function_name, event_content), silent = TRUE))
  if (inherits(response_object$result, "try-error")) {
    response_object$result <- NULL # TODO: We would like to return some more info about the error here
    response_object$status <- "failure"
  } else {
    response_object$status <- "success"
  }

  logger::log_debug("Result:", as.character(response_object$result))
  response_endpoint <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/", aws_request_id, "/response")

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

  rm("aws_request_id") # so we don't report errors to an outdated endpoint
}

logger::log_info("Querying for events")

while (TRUE) {
  tryCatch(
    {
      event <- httr::GET(url = next_invocation_endpoint)
      logger::log_debug("Event received")
      handle_event(event)
    },
    api_error = function(e) {
      logger::log_error(as.character(e))

      aws_request_id <- httr::headers(event)[["lambda-runtime-aws-request-id"]]

      if (exists("aws_request_id")) {
        logger::log_debug("POSTing invocation error for ID:", aws_request_id)

        invocation_error_endpoint <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/", aws_request_id, "/error")

        httr::POST(
          url = invocation_error_endpoint,
          body = list(
            statusCode = e$code,
            error_message = as.character(e$message)
          ),
          encode = "json"
        )
      } else {
        logger::log_debug("No invocation ID! Can't clear this request from the queue.")
      }
    },
    error = function(e) {
      logger::log_error(as.character(e))
      aws_request_id <- httr::headers(event)[["lambda-runtime-aws-request-id"]]
      if (exists("aws_request_id")) {
        logger::log_debug("POSTing invocation error for ID:", aws_request_id)
        invocation_error_endpoint <- paste0("http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/", aws_request_id, "/error")

        httr::POST(
          url = invocation_error_endpoint,
          body = list(error_message = as.character(e)),
          encode = "json"
        )
      } else {
        logger::log_debug("No invocation ID! Can't clear this request from the queue.")
      }
    }
  )
}
