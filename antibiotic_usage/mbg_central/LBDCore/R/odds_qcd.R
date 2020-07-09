#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param quantile_low PARAM_DESCRIPTION, Default: 5
#' @param quantile_high PARAM_DESCRIPTION, Default: 95
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname odds_qcd
#' @export
odds_qcd <- function(x, quantile_low = 5, quantile_high = 95) {

  # "quantile coefficient of dispersion, odds"
  low <- quantile_low / 100
  high <- quantile_high / 100
  quantiles <- quantile(x, c(low, high), na.rm = T)
  q_low <- as.numeric(quantiles[1])
  q_high <- as.numeric(quantiles[2])

  q_low <- q_low / (1 - q_low)
  q_high <- q_high / (1 - q_high)

  output <- (q_high - q_low) / (q_high + q_low)

  return(output)
}
