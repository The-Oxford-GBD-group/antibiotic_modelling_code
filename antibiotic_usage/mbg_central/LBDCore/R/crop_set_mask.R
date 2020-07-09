#' @title Crop and mark
#'
#' @description  Crop the raster to the extent of a simple_raster object,
#' set the extent, and then mask it based on the simple_raster object.
#'
#' @author Rebecca Stubbs
#'
#' @param raster_object A raster object (raster, Brick, etc) of a specific region
#' @param simple_raster A simple raster object that serves as the template for that region
#' @return A raster/Brick/Stack, with the extents and masks of the simple raster.
#' @export
crop_set_mask <- function(raster_object, template_raster) {
  raster_object <- raster::crop(raster_object, extent(template_raster))
  raster_object <- raster::setExtent(raster_object, template_raster)
  raster_object <- raster::mask(raster_object, template_raster)
  return(raster_object)
}
