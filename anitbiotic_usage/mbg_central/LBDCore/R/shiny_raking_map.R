#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname shiny_raking_map
#' @export
shiny_raking_map <- function() {



  # extract admin0
  if (exists("subset_shape") == FALSE) {
    message("Opening master shapefile because not found in global env...")
    master_shape <- shapefile(paste0(root, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp"))
    subset_shape <- master_shape[master_shape@data$GAUL_CODE %in% gaul_list, ]
  }
  admin0.dt <- data.table(fortify(subset_shape))


  if (time_stamp == TRUE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  if (time_stamp == FALSE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/scratch")
  plot_dir <- paste0(output_dir, "/plots")
  dir.create(plot_dir, showWarnings = FALSE)

  names(rf)[names(rf) == "name"] <- "GAUL_CODE"
  rf$raking_factor[is.infinite(rf$raking_factor)] <- -1

  # Loop over periods
  gg.count <- 1
  for (period in unique(rf$year)) {
    rf_shape <- merge(subset_shape, rf[rf$year == period, ], by = "GAUL_CODE")
    rf_shape@data$id <- rownames(rf_shape@data)
    rf.pts <- fortify(rf_shape, region = "id")
    rf.df <- join(rf.pts, rf_shape@data, by = "id")
    rf.df$raking_factor[is.na(rf.df$raking_factor)] <- 0
    rf.df$raking_factor <- as.numeric(rf.df$raking_factor)

    # Plot
    color_list <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
    admin2.gg <- ggplot(rf.df, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = raking_factor)) +
      geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "black", size = .5) +
      scale_fill_gradientn(colours = color_list, limits = c(0, max(rf$raking_factor[!is.na(rf$raking_factor)])), na.value = "#000000") +
      guides(fill = guide_colorbar(label = TRUE, ticks = FALSE)) +
      ggtitle(period) +
      scale_x_continuous("", breaks = NULL) +
      scale_y_continuous("", breaks = NULL) +
      coord_equal()
    # grab your legends using the predefined functions, then state their grid location
    p.legend <- gLegend(admin2.gg)
    assign(paste0("ggplot.", gg.count), admin2.gg)
    gg.count <- gg.count + 1
  }

  # Make data and preds pngs for Shiny
  png(paste0(plot_dir, "/rf_map1.png"), width = 400)
  print(ggplot.1)
  dev.off()
  png(paste0(plot_dir, "/rf_map2.png"), width = 400)
  print(ggplot.2)
  dev.off()
  png(paste0(plot_dir, "/rf_map3.png"), width = 400)
  print(ggplot.3)
  dev.off()
  png(paste0(plot_dir, "/rf_map4.png"), width = 400)
  print(ggplot.4)
  dev.off()
}
