#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ntv_plot_function
#' @export
ntv_plot_function <- function(x) {
  covs.sp <- rasterToPoints(nt_covs[[x]], spatial = TRUE)
  projection <- proj4string(covs.sp)

  # reproject sp object
  covs.sp <- spTransform(covs.sp, CRS(projection))
  covs.sp@data <- data.frame(covs.sp@data, long = coordinates(covs.sp)[, 1], lat = coordinates(covs.sp)[, 2])
  covs.dt <- data.table(covs.sp@data)

  ## Plot preds of proportion with 0 years of education
  names(covs.dt)[names(covs.dt) == "lat"] <- "latitude"
  names(covs.dt)[names(covs.dt) == "long"] <- "longitude"

  covs.gg <- ggplot(covs.dt, aes(longitude, latitude)) +
    geom_raster(aes(fill = get(x))) +
    coord_fixed() +
    theme_minimal() +
    geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "white", lwd = .1) +
    scale_fill_gradientn(colours = (color_list), limits = c(minValue(nt_covs[[x]]), maxValue(nt_covs[[x]])), na.value = "grey") +
    guides(fill = guide_colorbar(title = x, label = TRUE, ticks = FALSE)) +
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  return(covs.gg)
}
