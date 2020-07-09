#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param run_time PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname format_time
#' @export
format_time <- function(run_time) {
  run_time <- round(as.numeric(run_time), 0)

  hours <- run_time %/% 3600
  remainder <- run_time %% 3600
  minutes <- remainder %/% 60
  seconds <- remainder %% 60

  run_time <- paste0(hours, "h ", minutes, "m ", seconds, "s")
  return(run_time)
}
