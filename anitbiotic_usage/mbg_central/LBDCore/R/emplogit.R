# Take the emperical logit
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param success PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION
#' @param epsilon PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname emplogit
#' @export
emplogit <- function(success, N, epsilon = NULL) {
  # http://stats.stackexchange.com/questions/109702/empirical-logit-transformation-on-percentage-data
  # squeeze in the edges
  tform <- success / N

  # if epsilon is null, follow the instructions from the attached link
  if (is.null(epsilon)) {
    epsilon <- min(tform[tform > 0 & tform < 1]) / 2
  }

  tform[tform == 0] <- tform[tform == 0] + epsilon
  tform[tform == 1] <- tform[tform == 1] - epsilon
  tform <- log(tform / (1 - tform))

  return(tform)
}
