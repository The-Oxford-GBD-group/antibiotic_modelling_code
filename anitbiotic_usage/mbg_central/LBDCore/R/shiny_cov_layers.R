#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fixed_effects PARAM_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname shiny_cov_layers
#' @export
shiny_cov_layers <- function(fixed_effects, gaul_list, run_date, indicator, indicator_group) {

  # Plot covariate layers

  color_list <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")

  # Make sure covs are loaded
  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, ".RData"))

  # Load actual data (df already in memory)
  if (time_stamp == TRUE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  if (time_stamp == FALSE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/scratch")
  plot_dir <- paste0(output_dir, "/plots")
  dir.create(plot_dir, showWarnings = FALSE)

  # Get templates
  if (exists("subset_shape") == FALSE) {
    message("Opening master shapefile because not found in global env...")
    master_shape <- shapefile(paste0(root, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp"))
    subset_shape <- master_shape[master_shape@data$GAUL_CODE %in% gaul_list, ]
  }
  admin0.dt <- data.table(fortify(subset_shape))

  # Plot varying covariates
  selected_covs <- strsplit(fixed_effects, " ")
  selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]

  tv_count <- 0
  for (c in selected_covs) {
    if (paste0("tv_", c) %in% grep("tv_*", ls(), value = TRUE)) {
      tv_count <- tv_count + 1
      tv_cov <- get(paste0("tv_", c))

      # Convert raster to SpatialPointsDataFrame
      preds.sp <- rasterToPoints(tv_cov, spatial = TRUE)
      projection <- proj4string(preds.sp)

      # reproject sp object
      preds.sp <- spTransform(preds.sp, CRS(projection))
      preds.sp@data <- data.frame(preds.sp@data, long = coordinates(preds.sp)[, 1], lat = coordinates(preds.sp)[, 2])
      preds.dt <- data.table(preds.sp@data)

      ## Plot preds of proportion with 0 years of education
      names(preds.dt)[names(preds.dt) == "lat"] <- "latitude"
      names(preds.dt)[names(preds.dt) == "long"] <- "longitude"

      # Plot predictions for all periods
      for (i.period in 1:4) {
        assign(paste0("tv_cov_", tv_count, ".gg.", i.period), plot.preds(paste0(c, ".", i.period)))
      }
    }
  }

  # Plot all varying covariates
  png(paste0(plot_dir, "/tv_covs.png"), width = 1600, height = 1600)
  total_rows <- tv_count * 5
  # Initialize plot with master title
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(total_rows, 22, heights = c(.25, .25, .25, .25), widths = c(.25, .25, .25, .25))))

  # Plot all data coverage maps
  start_row <- 1
  for (i in 1:tv_count) {
    end_row <- start_row + 4
    print(get(paste0("tv_cov_", i, ".gg.1")) + theme(legend.position = "none"), vp = vplayout(start_row:end_row, 1:5))
    print(get(paste0("tv_cov_", i, ".gg.2")) + theme(legend.position = "none"), vp = vplayout(start_row:end_row, 6:10))
    print(get(paste0("tv_cov_", i, ".gg.3")) + theme(legend.position = "none"), vp = vplayout(start_row:end_row, 11:15))
    print(get(paste0("tv_cov_", i, ".gg.4")) + theme(legend.position = "none"), vp = vplayout(start_row:end_row, 16:20))
    p.legend <- gLegend(get(paste0("tv_cov_", i, ".gg.1")))
    p.legend$vp <- viewport(layout.pos.row = start_row:end_row, layout.pos.col = 21:22)
    grid.draw(p.legend)
    start_row <- start_row + 5
  }
  dev.off()

  nt_covs <- covs # non-varying


  list_of_ntv_ggs <- lapply(names(nt_covs), ntv_plot_function)

  # Plot all non-varying covariates
  png(paste0(plot_dir, "/ntv_covs.png"), width = 1200)
  total_columns <- length(names(nt_covs)) * 6
  # Initialize plot with master title
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(5, total_columns, heights = c(.25, .25, .25, .25), widths = c(.25, .25, .25, .25))))
  # Plot all data coverage maps
  start_col <- 1
  for (i in 1:length(names(nt_covs))) {
    end_col <- start_col + 4
    print(list_of_ntv_ggs[[i]] + theme(legend.position = "none"), vp = vplayout(1:5, start_col:end_col))
    p.legend <- gLegend(list_of_ntv_ggs[[i]])
    p.legend$vp <- viewport(layout.pos.row = 1:5, layout.pos.col = end_col + 1)
    grid.draw(p.legend)
    start_col <- end_col + 2
  }
  dev.off()
}
