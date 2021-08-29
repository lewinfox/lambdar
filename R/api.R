#' Lambdar API responses
#'
#' This page sets out what your client application can expect to receive from a lambdar API call.
#'
#' @section Response format:
#' The API returns a JSON object the format of which varies depending on the outcome of the
#' function.
#'
#' ## Success
#'
#' If the function returned successfully with no warnings the result will look like this:
#'
#' ```
#' {
#'   "status": "ok",
#'   "result": "<whatever the result is>"
#' }
#' ````
#'
#' ## Success but the function raised warning/s
#'
#' If warnings are raised by the lambda function this will be noted in the response object:
#'
#' ```
#' {
#'   "status": "warning",
#'   "result" "<whatever the result is>",
#'   "warning_messages": "["warning_message_1", "warning_message_2", ...]"
#' }
#' ```
#'
#' ## Lambda function failed
#'
#' If the lambda function raised an error the response will look like this:
#'
#' ```
#' {
#'   "status": "error",
#'   "error_message": "<error message>"
#' }
#' ```
#'
#' ## Lambdar runtime failed
#'
#' If the lambdar runtime environment itself experienced an error, the response will look like this:
#'
#' ```
#' {
#'   "status": "runtime_error",
#'   "error_message": "<error message>"
#' }
#' ```
#'
#' @section Handling the response:
#' To handle the result, check the value of the `"status"` field.
#'
#' * `"ok"`: Function executed successfully and the value is in the `"result"` field ready for use.
#' * `"warning"`: Function executed but raised warnings. You can use the value in the `"result"`
#'   field but you may want to do extra checks.
#' * `"error"`: The lambda function raised an error and there is no `"result"`. Either something is
#'   wrong with your input or the lambda code.
#' * `"runtime_error"`: There was an error in the lambdar runtime (not your code). This is probably
#'   going to be an issue on the AWS end so you should probably just try the request again.
#'
#' @name lambdar-api
#' @aliases api
NULL
