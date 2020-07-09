#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param pred_file PARAM_DESCRIPTION
#' @param layer_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname shiny_data_and_preds
#' @export
shiny_data_and_preds <- function(gaul_list, run_date, indicator, indicator_group, pred_file, layer_name) {


  # Settings
  # color_list <- c("#000000","#00281D","#07425B","#38499A","#8149B9","#C653AF","#EB7190","#EC9F7D","#DCCF91","#DBF0C6")
  color_list <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  if (indicator == "edu_mean") color_list <- rev(color_list)

  # extract admin0
  if (exists("subset_shape") == FALSE) {
    message("Opening master shapefile because not found in global env...")
    master_shape <- shapefile(paste0(root, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp"))
    subset_shape <- master_shape[master_shape@data$GAUL_CODE %in% gaul_list, ]
  }
  admin0.dt <- data.table(fortify(subset_shape))


  # Load actual data (df already in memory)
  if (time_stamp == TRUE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  if (time_stamp == FALSE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/scratch")
  plot_dir <- paste0(output_dir, "/plots")
  dir.create(plot_dir, showWarnings = FALSE)
  message(paste0("saving plots to ", plot_dir))
  message(paste0("immediately viewable at http://mbg-viz.duckdns.org:3456/ under your indicator/run_date"))

  periods <- data.frame(group = rep(1:length(unique(df$year)), 5), years = rep(sort(unique(df$year)), 5))
  df$period <- match(df$year, periods$years) # add these to df

  # Make quantity of interest
  df <- as.data.table(df)
  if (indicator_family == "binomial") df <- df[, to_map := get(indicator) / N]
  if (indicator_family == "gaussian") df <- df[, to_map := get(indicator)]

  # Plot data cluster means for each year
  for (period in sort(unique(df$period))) {
    assign(paste("data.gg", period, sep = "."), plot.data(period))
  }

  f <- paste0(output_dir, "/", pred_file)
  preds <- brick(f)
  # preds <- setExtent(preds, subset_shape)

  # Convert raster to SpatialPointsDataFrame
  preds.sp <- rasterToPoints(preds, spatial = TRUE)
  projection <- proj4string(preds.sp)

  # reproject sp object
  preds.sp <- spTransform(preds.sp, CRS(projection))
  preds.sp@data <- data.frame(preds.sp@data, long = coordinates(preds.sp)[, 1], lat = coordinates(preds.sp)[, 2])
  preds.dt <- data.table(preds.sp@data)

  ## Plot preds of proportion with 0 years of education
  names(preds.dt)[names(preds.dt) == "lat"] <- "latitude"
  names(preds.dt)[names(preds.dt) == "long"] <- "longitude"

  # Plot predictions for all periods
  for (i.period in sort(unique(df$period))) {
    assign(paste("preds.gg", i.period, sep = "."), plot.preds(paste0(layer_name, i.period)))
  }

  # Make data and preds pngs for Shiny
  png(paste0(plot_dir, "/data1.png"), width = 400)
  print(data.gg.1)
  dev.off()
  png(paste0(plot_dir, "/data2.png"), width = 400)
  print(data.gg.2)
  dev.off()
  png(paste0(plot_dir, "/data3.png"), width = 400)
  print(data.gg.3)
  dev.off()
  png(paste0(plot_dir, "/data4.png"), width = 400)
  print(data.gg.4)
  dev.off()
  png(paste0(plot_dir, "/preds1.png"), width = 400)
  print(preds.gg.1)
  dev.off()
  png(paste0(plot_dir, "/preds2.png"), width = 400)
  print(preds.gg.2)
  dev.off()
  png(paste0(plot_dir, "/preds3.png"), width = 400)
  print(preds.gg.3)
  dev.off()
  png(paste0(plot_dir, "/preds4.png"), width = 400)
  print(preds.gg.4)
  dev.off()
}
