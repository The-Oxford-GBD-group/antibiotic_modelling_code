#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param this_nid PARAM_DESCRIPTION
#' @param estimates_raster PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname compile_polygons
#' @export
compile_polygons <- function(this_nid, estimates_raster) {
  ## Define function to extract preds/pops over comparison polygons, and calculate pop-weighted mean outcome over polygons.

  message(this_nid)
  nid_results <- compare_data[nid == this_nid, ]

  gaul <- gaul_convert(nid_results[, iso3][1], shapefile_version = shapefile_version)
  message(paste0("gaul", gaul))
  country_pops <- master_list[[paste0("list_", gaul, ".pops_", gaul)]]
  country_pops <- crop(country_pops, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
  country_pops <- setExtent(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
  country_pops <- mask(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
  message("crop")
  country_estimates <- crop(estimates_raster, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
  country_estimates <- setExtent(country_estimates, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
  country_estimates <- mask(country_estimates, master_list[[paste0("list_", gaul, ".simple_", gaul)]])

  all_data <- merge(compare_spdf, nid_results, by = c("location_code", "shapefile"))
  all_data <- all_data[!is.na(all_data@data$outcome), ]
  all_data$geo_mean <- 0
  for (shape_x in unique(all_data$location_code)) {
    message(shape_x)
    test_poly <- all_data[all_data$location_code == shape_x, ]
    period <- test_poly$year[1] - 2000 + 1
    preds <- extract(country_estimates[[period]], test_poly)
    pops <- extract(country_pops[[period]], test_poly)
    all_data$geo_mean[all_data$location_code == shape_x] <- weighted.mean(preds[[1]], pops[[1]], na.rm = T)
  }
  this_data <- as.data.table(all_data)
  return(this_data)
}
