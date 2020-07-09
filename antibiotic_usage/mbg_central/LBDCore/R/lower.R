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
#' @rdname lower
#' @export
lower <- function(x) {
  # Simply get and return a percentile
  output <- quantile(x, 2.5 / 100, na.rm = T)
  return(output)
}
