#' @title Load Custom Admin Raster
#'
#' @description takes in a shapefile path or an spdf object and converts it into a admin raster using the field variable
#'
#' @param shapefile_path character string - Full file path to shapefile to be used to make admin raster.
#' @param field character string - Field in shapefile to be used as value in raster.
#' @param simple_raster Simple raster to set extent and resolution of admin raster.
#' @param shapefile option to pass in shapefile rather than path to save some time. Default `NULL`
#'
#' @return Raster with resolution and extent the same as simple_raster, with shapefile field as value
#' @export
#'
#' @examples
#' \dontrun{
#' custom_admin_raster <- load_custom_admin_raster(shapefile_path, field, simple_raster)
#' }
#' 
load_custom_admin_raster <- function(shapefile_path, field, simple_raster, shapefile = NULL) {
  sr <- simple_raster

  if (is.null(shapefile)) {
    shapes <- rgdal::readOGR(shapefile_path)
  } else {
    shapes <- shapefile
  }

  # crop
  cropped_shapes <- crop(shapes, extent(sr), snap = "out")

  # ensure field used for raster is numeric and rename to raster_id
  names(cropped_shapes)[names(cropped_shapes) == field] <- "raster_id"

  # get the number of unique factors in shapefile field of interest
  factor_count <- length(unique(cropped_shapes$raster_id))
  # convert from factor to numeric
  cropped_shapes$raster_id <- as.numeric(as.character(cropped_shapes$raster_id))
  # get the number of unique numbers in shapefile field of interest
  number_count <- length(unique(cropped_shapes$raster_id))

  # ensure that no values were dropped in the conversion from factor to numeric
  if (factor_count != number_count) {
    stop("In load_custom_admin_raster(), conversion of shapefile field from factor to number resulted in a different number of unique values")
  }
  # rasterize shapefile - make sure field is numeric, not factor
  initial_raster <- rasterize_check_coverage(cropped_shapes, sr, field = "raster_id")

  masked_shapes <- raster::mask(x = initial_raster, mask = sr)

  return(masked_shapes)
}
