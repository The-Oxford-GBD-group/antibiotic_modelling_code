#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param error PARAM_DESCRIPTION
#' @param w PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname weighted.rmse
#' @export
weighted.rmse <- function(error, w) {
  sqrt(sum((w / sum(w)) * ((error)^2)))
}
