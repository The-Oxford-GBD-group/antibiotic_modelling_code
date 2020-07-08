#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param country_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname extract_year_preds
#' @export
extract_year_preds <- function(country_year) {
  period <- country_year - 2000 + 1
  input_data_year <- country_input_dt[year == country_year, ]
  preds_year <- results[[period]]
  preds_at_points <- raster::extract(preds_year, input_data_year[, c("longitude", "latitude"), with = F])
  input_data_year <- input_data_year[, pred := preds_at_points]
  return(input_data_year)
}
