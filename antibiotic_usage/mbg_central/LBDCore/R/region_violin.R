#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param output_file PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname region_violin
#' @export
region_violin <- function(indicator,
                          indicator_group,
                          run_date,
                          output_file) {
  results_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  preds <- raster(paste0(results_dir, "/", indicator, "_mean_raster.tif"))
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
  input_data <- rbindlist(lapply(c("essa", "wssa", "cssa", "sssa", "name"), extract_region))
  pdf(output_file)

  ggplot(data = input_data) + geom_violin(aes(x = variable, y = value, fill = region)) + facet_wrap(~region)
  dev.off()
}
