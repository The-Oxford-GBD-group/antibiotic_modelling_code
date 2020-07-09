#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_africa_raster
#' @export
make_africa_raster <- function(gaul) {

  # Convert raster to SpatialPointsDataFrame
  message(paste0("rasterizing ", gaul, "..."))
  pixel_probs <- fread(paste0(results_dir, "/pixels/", measure, "_", gaul, ".csv"))
  pixel_probs <- pixel_probs[year == 2015, ]
  simple_raster <- raster(paste0(results_dir, "/simple/", gaul, ".tif"))
  probs_raster <- insertRaster(simple_raster, matrix(pixel_probs[, mean], ncol = 1))
  return(probs_raster)
}
