#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: 'full_run.log'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname startLog
#' @export
startLog <- function(file = "full_run.log") {
  # create a logfile and start logging
  con <- file(file)
  sink(con,
    split = TRUE
  )
  sink(con,
    type = "message"
  )

  # report session info
  message(sprintf("# starting log at %s\n", Sys.time()))
  message("# session info:\n")
  print(sessionInfo())
  message("\n# run log:\n")
}
