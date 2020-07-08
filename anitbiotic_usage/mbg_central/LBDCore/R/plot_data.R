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
#' @rdname plot.data
#' @export
plot.data <- function(x) {
  df.period <- subset(df, period == x)
  df.year <- df$year[df$period == x][1]
  loop.data.gg <- ggplot() +
    geom_polygon(data = admin0.dt, aes(x = long, y = lat, group = group), fill = "grey90", color = "grey") +
    geom_point(data = df.period, aes(x = longitude, y = latitude, color = to_map), pch = 16, size = 1) +
    scale_color_gradientn(colours = rev(color_list), limits = c(min(df[, to_map]), max(df[, to_map])), na.value = "white") +
    guides(fill = guide_colorbar(title = indicator, label = TRUE, ticks = FALSE)) +
    coord_fixed() +
    ggtitle(df.year) +
    guides(size = FALSE) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) + theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines"))
  return(loop.data.gg)
}
