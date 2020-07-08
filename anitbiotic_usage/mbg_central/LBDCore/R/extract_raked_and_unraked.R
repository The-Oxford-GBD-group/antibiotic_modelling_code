#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @param results PARAM_DESCRIPTION
#' @param results_raked PARAM_DESCRIPTION
#' @param master_list PARAM_DESCRIPTION
#' @param admin0 PARAM_DESCRIPTION
#' @param load_simple_raster PARAM_DESCRIPTION, Default: FALSE
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname extract_raked_and_unraked
#' @export
extract_raked_and_unraked <- function(gaul,
                                      results,
                                      results_raked,
                                      master_list,
                                      admin0,
                                      load_simple_raster = FALSE,
                                      shapefile_version = "current") {
  if (load_simple_raster == TRUE) {
    gaul_list <- gaul
    simple_polygon_list <- load_simple_polygon(
      gaul_list = gaul_list,
      buffer = 0.4,
      subset_only = T,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    raster_list <- build_simple_raster_pop(subset_shape)
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]
  }
  if (load_simple_raster == FALSE) {
    simple_raster <- master_list[[paste0("list_", gaul, ".simple_", gaul)]]
  }
  results_subset <- crop(results, extent(simple_raster))
  results_subset <- setExtent(results_subset, simple_raster)
  results_subset <- mask(results_subset, simple_raster)

  raked_results_subset <- crop(results_raked, extent(simple_raster))
  raked_results_subset <- setExtent(raked_results_subset, simple_raster)
  raked_results_subset <- mask(raked_results_subset, simple_raster)

  # Convert raster to SpatialPointsDataFrame
  unraked_preds.sp <- rasterToPoints(results_subset, spatial = TRUE)
  projection <- proj4string(unraked_preds.sp)

  # reproject sp object
  unraked_preds.sp <- spTransform(unraked_preds.sp, CRS(projection))
  unraked_preds.sp@data <- data.frame(unraked_preds.sp@data, long = coordinates(unraked_preds.sp)[, 1], lat = coordinates(unraked_preds.sp)[, 2])
  unraked_preds.dt <- data.table(unraked_preds.sp@data)
  names(unraked_preds.dt)[names(unraked_preds.dt) == "lat"] <- "latitude"
  names(unraked_preds.dt)[names(unraked_preds.dt) == "long"] <- "longitude"

  color_list <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")
  results_limits <- c(
    min(c(minValue((results_subset)), minValue((raked_results_subset)))),
    max(c(maxValue((results_subset)), maxValue((raked_results_subset))))
  )

  admin0 <- admin0[admin0$ADM0_CODE %in% gaul, ]
  admin0.dt <- data.table(fortify(admin0))

  ## Reshape for the gg
  unraked_preds.dt <- melt(unraked_preds.dt, id.vars = c("longitude", "latitude"), measure.vars = grep(indicator, names(unraked_preds.dt), value = T))
  unraked_preds.dt <- unraked_preds.dt[, year := as.character(variable)]
  unraked_preds.dt <- unraked_preds.dt[, year := as.numeric(gsub(paste0(indicator, "_mean_raster."), "", year))]
  unraked_preds.dt <- unraked_preds.dt[year %in% c(1, 6, 11, 16), ]
  unraked_preds.dt <- unraked_preds.dt[, year := year + 2000 - 1]

  results_gg <- ggplot(unraked_preds.dt, aes(longitude, latitude)) +
    geom_raster(aes(fill = value)) +
    coord_fixed() +
    theme_minimal() +
    geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "black", lwd = .1) +
    scale_fill_gradientn(colours = (color_list), limits = results_limits, na.value = "#000000") +
    guides(fill = guide_colorbar(title = "Mean outcome", label = TRUE, ticks = FALSE)) +
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    facet_wrap(~year, ncol = 4)

  # Convert raster to SpatialPointsDataFrame
  raked_preds.sp <- rasterToPoints(raked_results_subset, spatial = TRUE)
  projection <- proj4string(raked_preds.sp)

  # reproject sp object
  raked_preds.sp <- spTransform(raked_preds.sp, CRS(projection))
  raked_preds.sp@data <- data.frame(raked_preds.sp@data, long = coordinates(raked_preds.sp)[, 1], lat = coordinates(raked_preds.sp)[, 2])
  raked_preds.dt <- data.table(raked_preds.sp@data)
  names(raked_preds.dt)[names(raked_preds.dt) == "lat"] <- "latitude"
  names(raked_preds.dt)[names(raked_preds.dt) == "long"] <- "longitude"

  ## Reshape for the gg
  raked_preds.dt <- melt(raked_preds.dt, id.vars = c("longitude", "latitude"), measure.vars = grep(indicator, names(raked_preds.dt), value = T))
  raked_preds.dt <- raked_preds.dt[, year := as.character(variable)]
  raked_preds.dt <- raked_preds.dt[, year := as.numeric(gsub(paste0(indicator, "_mean_raked_raster."), "", year))]
  raked_preds.dt <- raked_preds.dt[year %in% c(1, 6, 11, 16), ]
  raked_preds.dt <- raked_preds.dt[, year := year + 2000 - 1]

  raked_results_gg <- ggplot(raked_preds.dt, aes(longitude, latitude)) +
    geom_raster(aes(fill = value)) +
    coord_fixed() +
    theme_minimal() +
    geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "black", lwd = .1) +
    scale_fill_gradientn(colours = (color_list), limits = results_limits, na.value = "#000000") +
    guides(fill = guide_colorbar(title = "Mean outcome", label = TRUE, ticks = FALSE)) +
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    facet_wrap(~year, ncol = 4)

  all_plots <- list(results_gg, raked_results_gg)
  names(all_plots) <- c("unraked", "raked")
  return(all_plots)
}
