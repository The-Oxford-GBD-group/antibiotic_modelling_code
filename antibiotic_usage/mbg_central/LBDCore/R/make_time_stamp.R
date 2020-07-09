
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param time_stamp PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_time_stamp
#' @export
make_time_stamp <- function(time_stamp) {
  ## Make time stamp in standardized format.

  run_date <- gsub("-", "_", Sys.time())
  run_date <- gsub(":", "_", run_date)
  run_date <- gsub(" ", "_", run_date)

  if (time_stamp == FALSE) run_date <- "scratch"

  return(run_date)
}
