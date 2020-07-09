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
#' @rdname lock_file
#' @export
lock_file <- function(shp,
                      fast_shapefile_dir = "/share/geospatial/rds_shapefiles/") {

  # Lock a shapefile (e.g. while editing / updating)

  lockfile <- paste0(fast_shapefile_dir, ".", shp, "_lockfile.rds")

  new_lock <- data.table(
    user = Sys.info()["user"],
    time = Sys.time(),
    shape = shp
  )

  if (check_for_lock(shp) == T) wait_for_lock(shp = shp)

  if (file.exists(lockfile)) {
    locks <- readRDS(lockfile)
    locks <- rbind(locks, new_lock)
  } else if (!file.exists(lockfile)) {
    locks <- new_lock
  }

  invisible(saveRDS(locks, file = lockfile))
}
