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
#' @rdname plot.preds
#' @export
plot.preds <- function(x) {
  period <- strsplit(x, "[.]")[[1]][2]
  df.year <- df$year[df$period == period][1]
  loop.preds.gg <- ggplot(preds.dt, aes(longitude, latitude)) +
    geom_raster(aes(fill = get(x))) +
    coord_fixed() +
    theme_minimal() +
    geom_path(data = admin0.dt, aes(x = long, y = lat, group = group), color = "white", lwd = .1) +
    scale_fill_gradientn(colours = rev(color_list), limits = c(min(minValue(preds)), max_raked_value), na.value = "#a6d854") +
    guides(fill = guide_colorbar(title = indicator, label = TRUE, ticks = FALSE)) +
    scale_x_continuous("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    ggtitle(df.year) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  return(loop.preds.gg)
}
