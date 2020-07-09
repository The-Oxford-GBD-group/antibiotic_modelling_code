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
#' @rdname unlock_file
#' @export
unlock_file <- function(shp,
                        fast_shapefile_dir = "/share/geospatial/rds_shapefiles/") {

  # Unlock a file (e.g. if done with updating it)
  lockfile <- paste0(fast_shapefile_dir, ".", shp, "_lockfile.rds")

  if (!(file.exists(lockfile))) return(invisible())

  locks <- readRDS(lockfile)

  if (locks[shape == shp, user] != Sys.info()["user"]) {
    stop(paste0(
      "this file is locked by another user (",
      locks[shape == shp, user], ")"
    ))
  } else {
    locks <- subset(locks, shape != shp)
    if (nrow(locks) == 0) {
      unlink(lockfile)
    } else {
      invisible(saveRDS(locks, file = lockfile))
    }
  }
  return(invisible())
}
