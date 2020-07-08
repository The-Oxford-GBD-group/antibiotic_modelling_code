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
#' @rdname upper
#' @export
upper <- function(x) {
  # Simply get and return a percentile
  output <- quantile(x, 97.5 / 100, na.rm = T)
  return(output)
}
