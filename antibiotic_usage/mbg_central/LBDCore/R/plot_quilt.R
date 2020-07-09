#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param sample PARAM_DESCRIPTION
#' @param subset_shape PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot_quilt
#' @export
plot_quilt <- function(gaul_list, df, sample, subset_shape, shapefile_version = "current") { ## sample == "IS" or "OOS"

  df <- df[!is.na(get(sample)), ]
  subset_shape <- subset_shape[subset_shape$GAUL_CODE %in% gaul_list, ]
  df <- as.data.table(df)
  loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)
  setnames(df, "country", "ihme_lc_id")
  df <- merge(df, loc_names, by = "ihme_lc_id")
  df <- df[GAUL_CODE %in% gaul_list, ]
  if (length(df[, GAUL_CODE]) != 0) {
    quilt_list <- rbindlist(lapply(sort(unique(df[, year])), make_quilt_matrix))
    color_list <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")
    this_shape.dt <- data.table(fortify(subset_shape))
    redblue <- c("#313695", "#ffffff", "#a50026")
    # plot gg
    quilt_gg <- ggplot(quilt_list, aes(longitude, latitude)) +
      geom_raster(aes(fill = value)) +
      coord_fixed() +
      theme_minimal() +
      geom_path(data = this_shape.dt, aes(x = long, y = lat, group = group), color = "black", lwd = .1) +
      # scale_fill_gradientn(colours=(color_list), limits=c(min(df[, get(sample)]), max(df[, get(sample)])), na.value = "#000000") +
      scale_fill_gradientn(colours = redblue, values = c(min(df[, get(sample)]), 0, max(df[, get(sample)])), limits = c(min(df[, get(sample)]), max(df[, get(sample)])), na.value = "#000000", rescaler = function(x, ...) x, oob = identity) +
      guides(fill = guide_colorbar(title = "Absolute\nerror", label = TRUE, ticks = FALSE)) +
      scale_x_continuous("", breaks = NULL) +
      scale_y_continuous("", breaks = NULL) +
      theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      facet_wrap(~year)
    return(quilt_gg)
  }
  if (length(df[, GAUL_CODE]) == 0) {
    return(NULL)
  }
}
