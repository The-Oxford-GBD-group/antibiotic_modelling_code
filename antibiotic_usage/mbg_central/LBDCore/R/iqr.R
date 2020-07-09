#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param quantile_low PARAM_DESCRIPTION, Default: 25
#' @param quantile_high PARAM_DESCRIPTION, Default: 75
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname iqr
#' @export
iqr <- function(x, quantile_low = 25, quantile_high = 75) {

  # "interquantile" range - can specify
  low <- quantile_low / 100
  high <- quantile_high / 100
  quantiles <- quantile(x, c(low, high), na.rm = T)
  q_low <- as.numeric(quantiles[1])
  q_high <- as.numeric(quantiles[2])

  output <- (q_high - q_low)

  return(output)
}
