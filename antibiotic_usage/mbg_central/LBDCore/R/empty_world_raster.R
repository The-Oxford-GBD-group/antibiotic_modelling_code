#' @title Create raster representing world
#'
#' @description Creates a raster file consistent with LBD's world raster.
#'
#' @param whole_world Logical if TRUE return raster for entire world. If FALSE,
#'   return raster for stage1+stage2
#'
#' @export
empty_world_raster <- function(whole_world = FALSE) {
  if (whole_world) {
    result <- raster::raster(
      nrow = 4320, ncol = 8640,
      crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
      xmn = -180, xmx = 180, ymn = -90, ymx = 90,
      vals = NA
    )
  } else { # stage 1+2
    result <- raster::raster(
      nrow = 2123, ncol = 6610,
      crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
      xmn = -118.375, xmx = 157.0417, ymn = -34.875, ymx = 53.58333,
      vals = NA
    )
  }
  return(result)
}
