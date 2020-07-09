
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param simple PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_template_raster
#' @export
get_template_raster <- function(simple) {
  ## Make template rasters (pull from central analysis folders managed by Lucas based off the area to model specified in config)
  #   Arguments:
  #     simple = Single polygon that defines boundaries of the entire area you want to model over.
  #   Returns: Empty raster over modeling area. To be used for cropping covariates quickly and projecting model.

  message("Creating rasters of admin units")
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")

  # Centrally controlled folder of analysis shapefiles
  analysis_raster_dir <- paste0(root, "/WORK/11_geospatial/10_mbg/central_spatial_files/analysis_integer_rasters/")

  # Load empty raster for largest analysis area, mask/crop to selected analysis area
  template_raster <- raster(paste0(analysis_raster_dir, "stage2_analysis.tif"))
  template_raster <- mask(crop(template_raster, simple), simple)

  return(template_raster)
}
