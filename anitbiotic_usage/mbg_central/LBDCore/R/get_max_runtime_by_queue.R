#' @title Get max runtime of queue
#'
#' @description Check the input string of queue and the maximum runtime available for that queue
#'
#' @param queue String. User supplied queue
#'
#' @return a string of run-time in format \code{DD:HH:MM:SS}
#'
#' @export
get_max_runtime_by_queue <- function(queue) {
  if (queue %like% "all") {
    return("03:00:00:00")
  } else if (queue %like% "geospatial") {
    return("25:00:00:00")
  } else {
    return("16:00:00:00")
  }
}
