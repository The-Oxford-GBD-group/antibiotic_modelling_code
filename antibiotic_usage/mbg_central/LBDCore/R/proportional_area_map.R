#' @title Proportional Area Map
#' @description Create proportional area maps for count data at various admin levels
#'
#' @param data data frame or data table with at least a value and ADMX_CODE column
#' @param ad_level admin level to map (0,1,2)
#' @param value_col column in `data` holding the value to plot (proportional to size)
#' @param main_title main title for the map (default = NULL)
#' @param legend_title legend title for the map
#' @param fill_color color to use to fill in the proportional area circles
#' @param alpha transparency (alpha) of the proportional area circles
#' @param scale_size_max max size of the proportional area circles
#'                       (passed to `scale_size_area()`)
#' @param scale_size_breaks breaks to specify for size scale legend
#'                          (passed to `scale_size_area()`)
#' @param scale_size_breaks labels to specify for size scale legend
#'                          (passed to `scale_size_area()`)
#' @param lake_river_color controls the color of the lakes/rivers
#' @param out_file output file name (png)
#' @param out_file_height height of output file (inches)
#' @param out_file_width width of ouput file (inches)
#' @param out_file_res resolution of output file (dpi)
#' @param out_file_pointsize pointsize of text in output file (generally would first
#'                           adjust legend text/title size as below with the relevant
#'                           options, and then use this to modify line spacing, etc.)
#' @param legend_text_size adjust the size of the legend text
#' @param legend_title_size adjust the size of the legend title
#'
#' @return ggplot object containing the map object; writes png to `out_file` if not NULL
#' @examples
#' 
#' \dontrun{
#' a1_df <- fread("/share/geospatial/mbg/[ig]/[indic]/output/[rd]/pred_derivatives/admin_summaries/[indic]_admin_1_raked_summary.csv")
#' a1_df <- subset(a1_df, year == 2015)
#' 
#' gg_obj <- proportional_area_map(
#'   data = a1_df,
#'   ad_level = 1,
#'   value_col = "mean",
#'   main_title = NULL,
#'   legend_title = "DPT3: 2015",
#'   fill_col = "red",
#'   alpha = 0.25,
#'   size_scale_max = 5,
#'   out_file = "/my/out/file.png"
#' )
#' }
#' @export
proportional_area_map <- function(data, # Needs to have ADM0_CODE, ADM1_CODE, ADM2_CODE
                                  ad_level,
                                  value_col,
                                  main_title = NULL,
                                  legend_title,
                                  fill_col,
                                  alpha = 0.25,
                                  size_scale_max = 6,
                                  size_scale_breaks = NULL,
                                  size_scale_labels = NULL,
                                  lake_river_color = "lightblue",
                                  out_file = NULL,
                                  out_file_height = 12,
                                  out_file_width = 12,
                                  out_file_res = 300,
                                  out_file_pointsize = 16,
                                  legend_text_size = 16,
                                  legend_title_size = 18,
                                  shapefile_version = "current") {

  # Set up admin column code
  adm_code_col <- paste0("ADM", ad_level, "_CODE")

  # Copy input data to avoid environment/reference issues
  df <- copy(as.data.table(data))

  message("Loading background map...")
  message("Note: background map only available for Africa for now")
  background_shp <- readRDS("/share/geospatial/rds_shapefiles/background_map_africa/background_map_africa.rds")
  background_map <- fortify(background_shp)

  message("Loading master admin shape...")
  ad_shape <- rgdal::readOGR(get_admin_shapefile(ad_level, version = shapefile_version))
  if (ad_level == 2) {
    ad_shape <- subset(ad_shape, ADM2_CODE %in% unique(df[, get(adm_code_col)]))
  } else if (ad_level == 1) {
    ad_shape <- subset(ad_shape, ADM1_CODE %in% unique(df[, get(adm_code_col)]))
  } else if (ad_level == 0) {
    ad_shape <- subset(ad_shape, ADM0_CODE %in% unique(df[, get(adm_code_col)]))
  }

  message("Loading masks, lakes & rivers...")

  ### Load & process mask for ggplot2
  mask <- raster("/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/mask_master.tif")
  mask_df <- rasterToPoints(mask)
  mask_df <- data.frame(mask_df)
  colnames(mask_df) <- c("long", "lat", "mask")

  ### Load & process lakes & rivers for ggplot2
  lakes <- raster("/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/lakes_all_2.tif")
  lakes_df <- rasterToPoints(lakes)
  lakes_df <- data.frame(lakes_df)
  colnames(lakes_df) <- c("long", "lat", "lakes")

  message("Getting centroids...")
  if (ad_level == 2) centroids <- gCentroid(ad_shape, byid = T, id = ad_shape$ADM2_CODE)
  if (ad_level == 1) centroids <- gCentroid(ad_shape, byid = T, id = ad_shape$ADM1_CODE)
  if (ad_level == 0) centroids <- gCentroid(ad_shape, byid = T, id = ad_shape$ADM0_CODE)

  centroids <- as.data.frame(centroids) %>%
    cbind(rownames(.), .) %>%
    as.data.table(.) %>%
    setnames(., names(.), c("adm_code", "x", "y"))
  centroids$adm_code <- as.numeric(levels(centroids$adm_code))[centroids$adm_code]
  setnames(centroids, "adm_code", adm_code_col)

  message("Merging centroids to data")
  df <- merge(df, centroids, by = eval(adm_code_col), all.x = T, all.y = F)
  n_missing <- nrow(df[is.na(x) | is.na(y), ])

  if (n_missing > 0) {
    missing_ad_shapes <- unique(df[is.na(x) | is.na(y), get(adm_code_col)])
    message(paste0("The following admin shapes are missing: ", missing_ad_shapes))
  }

  message("Preparing to plot...")
  ad_shape_df <- fortify(ad_shape, region = adm_code_col)

  # A custom, mostly blank theme
  theme_empty <- theme_classic() +
    theme(
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # Set up an object to hold the scale size options
  if (is.null(size_scale_breaks) & is.null(size_scale_labels)) {
    size_scale_object <- scale_size_area(max_size = size_scale_max)
  } else if (!is.null(size_scale_breaks) & is.null(size_scale_labels)) {
    size_scale_object <- scale_size_area(
      max_size = size_scale_max,
      breaks = size_scale_breaks
    )
  } else if (is.null(size_scale_breaks) & !is.null(size_scale_labels)) {
    size_scale_object <- scale_size_area(
      max_size = size_scale_max,
      labels = size_scale_labels
    )
  } else if (!is.null(size_scale_breaks) & !is.null(size_scale_labels)) {
    size_scale_object <- scale_size_area(
      max_size = size_scale_max,
      breaks = size_scale_breaks,
      labels = size_scale_labels
    )
  }

  # For convenience change df name of plotting column
  setnames(df, value_col, "plot_me")

  message("Plotting...")
  g_map <- ggplot() +

    # Plot background
    geom_polygon(
      data = background_map,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "white"
    ) +

    # Plot national country outlines
    geom_path(
      data = background_map,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      size = 0.5,
      color = "black"
    ) +

    # Plot admin unit outlines
    geom_path(
      data = ad_shape_df,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      size = 0.3,
      color = "black"
    ) +

    # Plot lakes & rivers
    annotate(
      geom = "raster", x = lakes_df$long, y = lakes_df$lat,
      fill = lake_river_color,
      alpha = 0.75
    ) +

    # Plot points
    geom_point(
      data = df,
      aes(
        x = x,
        y = y,
        size = plot_me
      ),
      color = fill_col,
      alpha = alpha
    ) +

    size_scale_object +

    # Theme/style additions & labels
    theme_empty +
    coord_equal(ratio = 1) +
    labs(
      size = legend_title,
      main = title
    ) +
    guides(size = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.text = element_text(size = legend_text_size)) +
    theme(legend.title = element_text(size = legend_title_size))

  if (!is.null(out_file)) {
    message(paste0("Saving to ", out_file))
    png(
      filename = out_file,
      type = "cairo",
      units = "in",
      width = out_file_width,
      height = out_file_height,
      pointsize = out_file_pointsize,
      res = out_file_res
    )

    print(g_map)
    dev.off()
  }

  return(g_map)
}
