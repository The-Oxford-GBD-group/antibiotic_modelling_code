#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION
#' @param year_plot_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname raking_factors_map
#' @export
raking_factors_map <- function(indicator,
                               indicator_group,
                               run_date,
                               gaul,
                               shapefile_version,
                               year_plot_list) {


  # extract admin0
  master_shape <- readOGR(dsn = "/snfs1/temp/learl/data_coverage", layer = "africa_ad0")
  names(master_shape)[names(master_shape) == "ADM0_CODE"] <- "GAUL_CODE"
  gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
  master_shape <- merge(master_shape, gaul_to_loc_id, by = "GAUL_CODE")
  subset_shape <- master_shape[master_shape@data$GAUL_CODE %in% gaul, ]
  admin0.dt <- data.table(fortify(subset_shape))

  rf <- fread(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_rf.csv"))
  names(rf)[names(rf) == "name"] <- "GAUL_CODE"
  rf$raking_factor[is.infinite(rf$raking_factor)] <- -1
  rf <- rf[GAUL_CODE %in% gaul, ]

  # Loop over periods
  all_rf_df <- rbindlist(lapply(year_plot_list, make_period_matrix))

  # Plot
  color_list <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  redblue <- c("#313695", "#ffffff", "#a50026")
  admin2.gg <- ggplot(all_rf_df, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = raking_factor)) +
    geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "black", size = .5) +
    scale_fill_gradientn(colours = redblue, values = c(0.5, 1, 2), limits = c(0.5, 2), na.value = "#000000", rescaler = function(x, ...) x, oob = identity) +
    guides(fill = guide_colorbar(label = TRUE, ticks = FALSE)) +
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    guides(fill = guide_legend(title = "Raking\nfactor")) +
    coord_equal() +
    theme_minimal() +
    facet_wrap(~year, ncol = 4)
  return(admin2.gg)
}
