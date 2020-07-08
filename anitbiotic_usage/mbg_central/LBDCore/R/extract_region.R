#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname extract_region
#' @export
extract_region <- function(region) {
  input_data <- fread(paste0(results_dir, "/input_data_bin0_", region, "_0.csv"))
  input_data <- input_data[weight == 1 & N >= 20, ]
  preds_at_points <- extract(preds, input_data[, c("longitude", "latitude"), with = F])
  input_data <- input_data[, pred := preds_at_points]
  input_data <- input_data[!is.na(pred), ]
  input_data <- input_data[, data := get(indicator) / N]
  input_data <- input_data[, c("latitude", "longitude", "pred", "data"), with = F]
  input_data <- melt(input_data, id.vars = c("longitude", "latitude"), measure.vars = c("data", "pred"))
  input_data <- input_data[, region := region]
}
