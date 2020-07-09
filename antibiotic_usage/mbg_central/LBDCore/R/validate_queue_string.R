#' @title Validate user-supplied queue string
#'
#' @description Check the input string of queue and validate whether its a legitimate queue or not.
#' This checks for whether the queue values are matching with \code{all}, \code{geospatial} or \code{long}
#'
#' @param queue String. User supplied queue.
#'
#' @return the queue post-validating
#'
#' @export
validate_queue_string <- function(queue) {
  stopifnot(queue %in% c("all.q", "geospatial.q", "long.q"))
  return(queue)
}
