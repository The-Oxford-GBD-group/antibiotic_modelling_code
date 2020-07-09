#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname stopLog
#' @export
stopLog <- function() {
  # report session info
  message("\n# session info:\n")
  print(sessionInfo())
  message(sprintf("\n# stopping log at %s", Sys.time()))
  # stop logging to the logfile
  sink()
  sink(type = "message")
}
