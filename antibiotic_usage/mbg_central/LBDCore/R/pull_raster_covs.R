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
#' @rdname pull_raster_covs
#' @export
pull_raster_covs <- function(region) {
  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, "_bin0_", region, "_0.RData"))
  cov_raster <- cov_list[[cov]]
  return(cov_raster)
}
