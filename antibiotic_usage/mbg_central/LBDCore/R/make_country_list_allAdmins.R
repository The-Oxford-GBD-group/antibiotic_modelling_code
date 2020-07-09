#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_country_list_allAdmins
#' @export
make_country_list_allAdmins <- function(gaul) {
  message(gaul)
  # Save draws
  country_draws <- draws_by_gaul[GAUL_CODE == gaul, ]
  country_draws <- country_draws[, GAUL_CODE := NULL]
  # Save a template raster and population raster
  gaul_list <- gaul
  country_shape <- subset_shape[subset_shape@data$GAUL_CODE %in% gaul_list, ]
  raster_list <- build_simple_raster_pop(country_shape)
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]
  # Get population brick for all periods
  pop_raster_annual <- all_region_pops
  pop_raster_annual <- crop(pop_raster_annual, extent(simple_raster))
  pop_raster_annual <- setExtent(pop_raster_annual, simple_raster)
  pop_raster_annual <- mask(pop_raster_annual, simple_raster)
  # Get raster of admin2 codes
  admin_level <- 2
  shapes <- admin2_shapes
  cropped_shapes <- crop(shapes, extent(simple_raster), snap = "out")
  ## Fix rasterize
  initial_raster <- rasterize_check_coverage(cropped_shapes, simple_raster, field = paste0("ADM", admin_level, "_CODE"))
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) != 0) {
    rasterized_shape <- merge(rasterize_check_coverage(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ], simple_raster, field = paste0("ADM", admin_level, "_CODE")), initial_raster)
  }
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) == 0) {
    rasterized_shape <- initial_raster
  }
  admin2_masked_shapes <- mask(x = rasterized_shape, mask = simple_raster)
  # Get raster of admin1 codes
  admin_level <- 1
  shapes <- admin1_shapes
  cropped_shapes <- crop(shapes, extent(simple_raster), snap = "out")
  ## Fix rasterize
  initial_raster <- rasterize_check_coverage(cropped_shapes, simple_raster, field = paste0("ADM", admin_level, "_CODE"))
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) != 0) {
    rasterized_shape <- merge(rasterize_check_coverage(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ], simple_raster, field = paste0("ADM", admin_level, "_CODE")), initial_raster)
  }
  if (length(cropped_shapes[!cropped_shapes[[paste0("ADM", admin_level, "_CODE")]] %in% unique(initial_raster), ]) == 0) {
    rasterized_shape <- initial_raster
  }
  admin1_masked_shapes <- mask(x = rasterized_shape, mask = simple_raster)
  # Add items to list
  return_list <- list(
    country_draws,
    pop_raster_annual,
    simple_raster,
    admin2_masked_shapes,
    admin1_masked_shapes
  )
  names(return_list) <- c(paste0("draws_", gaul), paste0("pops_", gaul), paste0("simple_", gaul), paste0("admin2_", gaul), paste0("admin1_", gaul))
  return(return_list)
}
