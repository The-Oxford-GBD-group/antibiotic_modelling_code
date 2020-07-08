#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname grab_time_hours
#' @export
grab_time_hours <- function(x) {
  v_time <- unlist(strsplit(x, " "))
  hours <- v_time[1]
  hours <- substr(hours, 0, nchar(hours) - 1) %>% as.numeric()

  minutes <- v_time[2]
  minutes <- substr(minutes, 0, nchar(minutes) - 1) %>% as.numeric()

  seconds <- v_time[1]
  seconds <- substr(seconds, 0, nchar(seconds) - 1) %>% as.numeric()

  hours <- hours + minutes / 60 + seconds / (60 * 60)
  return(hours)
}
