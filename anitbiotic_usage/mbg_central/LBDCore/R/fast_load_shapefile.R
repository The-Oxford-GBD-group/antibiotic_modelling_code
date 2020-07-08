#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shape PARAM_DESCRIPTION
#' @param fast_shapefile_dir PARAM_DESCRIPTION, Default: '/share/geospatial/rds_shapefiles/'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fast_load_shapefile
#' @export
fast_load_shapefile <- function(shape,
                                fast_shapefile_dir = "/share/geospatial/rds_shapefiles/") {
  # Simple function to quickly load a shapefile

  wait_for_lock(shape)
  return(readRDS(paste0(fast_shapefile_dir, shape, ".rds")))
}
