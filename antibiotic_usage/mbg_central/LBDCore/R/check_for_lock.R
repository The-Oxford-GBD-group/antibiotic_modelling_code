#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shp PARAM_DESCRIPTION
#' @param fast_shapefile_dir PARAM_DESCRIPTION, Default: '/share/geospatial/rds_shapefiles/'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname check_for_lock
#' @export
check_for_lock <- function(shp,
                           fast_shapefile_dir = "/share/geospatial/rds_shapefiles/") {
  # Check to see if the file is locked

  lockfile <- paste0(fast_shapefile_dir, ".", shp, "_lockfile.rds")

  if (!file.exists(lockfile)) return(FALSE)

  locks <- readRDS(lockfile)
  if (shp %in% locks$shape) {
    return(TRUE)
  } else if (!(shp %in% locks$shape)) {
    return(FALSE)
  }
}
