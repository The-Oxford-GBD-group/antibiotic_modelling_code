## Make map of period indices to run any time periods in your data (like annual instead of 5-year)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param modeling_periods PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_period_map
#' @export
make_period_map <- function(modeling_periods) {
  data_period <- sort(modeling_periods)
  period_ids <- seq(data_period)
  period_map <- as.data.table(data_period)
  period_map <- period_map[, period_id := period_ids]
  return(period_map)
}
