
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param admin_level PARAM_DESCRIPTION
#' @param simple_raster PARAM_DESCRIPTION
#' @param disag_fact PARAM_DESCRIPTION, Default: NULL
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_admin_raster
#' @export
load_admin_raster <- function(admin_level, simple_raster, disag_fact = NULL,
                              shapefile_version = "current") {

  #################################################################################
  ### load admin raster
  ## Inputs:
  # admin_level: 0,1, or 2 are accepted.
  # disag_fact: factor to increase resolution (needed whith small districts), 50 makes it 100m
  # simple_raster: keeps resolution and extent
  # shapefile_version: string indicating which version of shapefile to pull
  ## Outputs: returns raster layer with gaul codes for values
  ## Notes:
  # Will need to update location once lucas gives these a permanent home
  #################################################################################

  if (!admin_level %in% c(0, 1, 2)) stop("admin_level must be either 0, 1, or 2")

  # load admin raster
  # adm <- raster(sprintf('%stemp/geospatial/U5M_africa/data/clean/shapefiles/ad%i_raster.grd',root,admin_level))
  if (!is.null(disag_fact)) {
    sr <- disaggregate(simple_raster, fact = disag_fact)
  } else {
    sr <- simple_raster
  }

  # UPDATED: master gaul admin shapefiles
  shapes <- shapefile(get_admin_shapefile(admin_level, version = shapefile_version))

  # The variable we rasterize on must be numeric.
  shapes@data[[paste0("ADM", admin_level, "_CODE")]] <- as.numeric(as.character(shapes@data[[paste0("ADM", admin_level, "_CODE")]]))

  # crop
  cropped_shapes <- crop(shapes, extent(sr), snap = "out")

  ## Fix rasterize
  initial_raster <- rasterize_check_coverage(cropped_shapes, sr, field = paste0("ADM", admin_level, "_CODE"))
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) != 0) {
    rasterized_shape <- merge(rasterize_check_coverage(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ], sr, field = paste0("ADM", admin_level, "_CODE")), initial_raster)
  }
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) == 0) {
    rasterized_shape <- initial_raster
  }
  # rasterized_shape <- rasterize_check_coverage(cropped_shapes, sr, field=paste0('ADM', admin_level,'_CODE'))
  masked_shapes <- mask(x = rasterized_shape, mask = sr)

  return(masked_shapes)
}
