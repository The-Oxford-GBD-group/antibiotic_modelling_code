#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname process_betas
#' @export
process_betas <- function(reg) {
  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_model_eb_bin0_", reg, "_0.RData"))
  model_data <- as.data.table(res_fit$summary.fixed)
  names(model_data)[names(model_data) == "0.025quant"] <- "lower"
  names(model_data)[names(model_data) == "0.975quant"] <- "upper"
  model_data[, cov := row.names(res_fit$summary.fixed)]
  model_data <- model_data[cov != "int"]
  model_data[, region := reg]
  return(model_data)
}
