#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param this_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fields]{quilt.plot}}
#' @rdname make_quilt_matrix
#' @export
#' @importFrom fields quilt.plot
make_quilt_matrix <- function(this_year) {

  ## Run quilt function from fields library, suppress inline plot
  df_year <- df[year == this_year, ]

  if (nrow(unique(df_year[, c("longitude", "latitude")])) > 1) {
    # checks to see if there are at least 2 points, otherwise quilt.plot fails
    quilt_x <- df_year[, longitude]
    quilt_y <- df_year[, latitude]
    quilt_z <- df_year[, get(sample)]
    pdf("NULL")
    quilt <- fields::quilt.plot(quilt_x, quilt_y, quilt_z, main = "Absolute error")
    dev.off()

    ## Pull spatial info from quilt object
    long_matrix <- melt(quilt$z) # Make matrix of values from quilt.plot long df
    z_raster <- rasterFromXYZ(long_matrix) # Convert first two columns as lon-lat and third as value
    proj4string(z_raster) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    z_raster <- setExtent(z_raster, extent(min(quilt$x), max(quilt$x), min(quilt$y), max(quilt$y)))

    # Convert raster to SpatialPointsDataFrame
    z_raster.sp <- rasterToPoints(z_raster, spatial = TRUE)
    projection <- proj4string(z_raster.sp)

    # reproject sp object
    z_raster.sp <- spTransform(z_raster.sp, CRS(projection))
    z_raster.sp@data <- data.frame(z_raster.sp@data, long = coordinates(z_raster.sp)[, 1], lat = coordinates(z_raster.sp)[, 2])
    z_raster.dt <- data.table(z_raster.sp@data)
    names(z_raster.dt)[names(z_raster.dt) == "lat"] <- "latitude"
    names(z_raster.dt)[names(z_raster.dt) == "long"] <- "longitude"
    z_raster.dt <- z_raster.dt[, year := this_year]
  } else {
    z_raster.dt <- data.table(
      value = numeric(),
      longitude = numeric(),
      latitude = numeric(),
      year = numeric()
    )
  }
  return(z_raster.dt)
}
