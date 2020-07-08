#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param percentile PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_percentile
#' @export
get_percentile <- function(x, percentile) {

  # Simply get and return a percentile
  percentile <- as.numeric(percentile)
  output <- quantile(x, percentile / 100, na.rm = T)
  return(output)
}
