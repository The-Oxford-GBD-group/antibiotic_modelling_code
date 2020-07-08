#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param subset_shape PARAM_DESCRIPTION
#' @param u5m PARAM_DESCRIPTION, Default: FALSE
#' @param field PARAM_DESCRIPTION, Default: NULL
#' @param raking PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{mask}}
#' @rdname build_simple_raster_pop
#'
#' @export
#'
#' @importFrom raster mask unique merge
#'
build_simple_raster_pop <- function(subset_shape, u5m = FALSE, field = NULL, raking = F, link_table = modeling_shapefile_version) {
  ## Load list of raster inputs (pop and simple)

  if (is.null(field)) {
    if ("GAUL_CODE" %in% names(subset_shape@data)) field <- "GAUL_CODE"
    if ("ADM0_CODE" %in% names(subset_shape@data)) field <- "ADM0_CODE"
  }

  if (raking) {
    field <- "loc_id"
    # no 'loc_id' field in the link table, so we can't use it
    link_table <- NULL
  }

  if (u5m == FALSE) {
    master_pop <- brick("/snfs1/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif") # WorldPop_allStages_stack.tif')
  } else {
    master_pop <- brick(
      raster("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2000_00_00.tif"),
      raster("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2005_00_00.tif"),
      raster("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2010_00_00.tif"),
      raster("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/archive/replaced_20170623/a0004t/5y/worldpop_a0004t_5y_2015_00_00.tif")
    )
  }

  cropped_pop <- crop(master_pop, extent(subset_shape), snap = "out")
  ## Fix rasterize
  initial_raster <- rasterize_check_coverage(subset_shape, cropped_pop, field = field, link_table = link_table)
  if (length(subset(subset_shape, !(get(field) %in% unique(initial_raster)))) != 0) {
    rasterized_shape <- 
      raster::merge(
        rasterize_check_coverage(subset(subset_shape, !(get(field) %in% unique(initial_raster))),
                                 cropped_pop,
                                 field = field,
                                 link_table = link_table),
        initial_raster)
  }
  if (length(subset(subset_shape, !(get(field) %in% unique(initial_raster)))) == 0) {
    rasterized_shape <- initial_raster
  }
  masked_pop <- raster::mask(x = cropped_pop, mask = rasterized_shape)

  raster_list <- list()
  raster_list[["simple_raster"]] <- rasterized_shape
  raster_list[["pop_raster"]] <- masked_pop

  return(raster_list)
}
