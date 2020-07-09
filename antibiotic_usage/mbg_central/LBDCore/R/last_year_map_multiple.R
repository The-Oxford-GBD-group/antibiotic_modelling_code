#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shape PARAM_DESCRIPTION
#' @param centroids PARAM_DESCRIPTION
#' @param admin PARAM_DESCRIPTION, Default: 0
#' @param year PARAM_DESCRIPTION, Default: 2015
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname last_year_map_multiple
#' @export
last_year_map_multiple <- function(shape, centroids, admin = 0, year = 2015) {
  # Last year maps if multiple runs are provided, this changes the fill to grey

  # Requirements and custom theme
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

  gg <- ggplot() +
    geom_polygon_quiet(
      data = shape,
      aes(
        x = long, y = lat,
        group = group
      ), fill = "grey"
    ) +
    geom_path_quiet(
      data = shape,
      aes(x = long, y = lat, group = group)
    ) +
    coord_equal() +
    theme_empty

  if (admin == 0 & nrow(centroids) > 25) {
    gg <- gg + geom_label_repel(
      data = centroids,
      aes(
        x = x,
        y = y,
        label = ihme_lc_id
      ),
      point.padding = unit(0.01, "lines"),
      box.padding = unit(1.5, "lines"),
      min.segment.length = unit(0, "lines"),
      segment.alpha = 0.5
    )
  } else {
    label <- paste0("ADM", admin, "_NAME")
    gg <- gg + geom_label_repel(
      data = centroids,
      aes(
        x = x,
        y = y,
        label = get(label)
      ),
      point.padding = unit(0.01, "lines"),
      box.padding = unit(1.5, "lines"),
      min.segment.length = unit(0, "lines"),
      segment.alpha = 0.5
    )
  }
  return(gg)
}
