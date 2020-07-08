#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shp PARAM_DESCRIPTION
#' @param fast_shapefile_dir PARAM_DESCRIPTION, Default: '/share/geospatial/rds_shapefiles/'
#' @param tries PARAM_DESCRIPTION, Default: 20
#' @param interval PARAM_DESCRIPTION, Default: 30
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname wait_for_lock
#' @export
wait_for_lock <- function(shp,
                          fast_shapefile_dir = "/share/geospatial/rds_shapefiles/",
                          tries = 20,
                          interval = 30) {
  # Wrapper for check_for_lock that checks every `interval` seconds up to `tries` attempts
  #   If unable to obtain the lock in that time period, will throw a `stop()`
  #   Useful in case someone is trying to modify a shapefile that you want to read/write


  for (i in 1:tries) {
    if (check_for_lock(shp) == F) invisible(return())
    Sys.sleep(interval)
  }

  lockfile <- paste0(fast_shapefile_dir, ".", shp, "_lockfile.rds")
  locks <- readRDS(lockfile)
  locks[shape == shp, user]

  stop(paste0(
    "shape ", shp,
    " locked by user ", locks[shape == shp, user],
    " at ", locks[shape == shp, time]
  ))
}
