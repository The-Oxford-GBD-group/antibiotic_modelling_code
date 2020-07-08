#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param theta PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname transform_theta
#' @export
transform_theta <- function(theta) {
  return((exp(theta) - 1) / (1 + exp(theta)))
}
