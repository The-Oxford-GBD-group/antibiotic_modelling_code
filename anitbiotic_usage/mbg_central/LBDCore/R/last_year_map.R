#' @title Labeled map
#' @description Create a labeled map of the admin levels included in the region/country/admin1 level. The labels included are the
#' specified admin names unless over 25 geographies are included, in which case they are labeled by ihme loc id. This is done so larger regions
#' like Africa do not print out the entire country name, as it would take up too much space.
#'
#' This map comes in two flavors:
#' 1) last_year_map: This map colors the region/country/admin1 level by your indicator with the scale specified by val_range (default 0-1)
#' 2) last_year_map_multiple : When multiple model indicators/runs are included, the map is no longer colored by indicator and instead
#' all grey.
#'
#' This function is called in the subnational_ts_function and is pretty specific to this function. However, it could also serve as a
#' template for generalizing a labeled map function that also colors by a specific indicator.
#'
#' @author Michael Cork, \email{mcork23@uw.edu}
#'
#' @param shape SpatialPolygonsDataFrame specified to the region/country/admin1 level that your model was run over
#' @param centroids Data table that species the x and y coordinates of the centroids for the shape of the map
#'                  and has the attached labels to display using ggrepel.
#' @param admin The specified admin level called in subnational_ts_function
#' @param val_range The specified value range for mean value of your indicator
#' @param distiller_direction Either 1 or -1, if high is bad, distiller direction is -1 with the red tones at high values of indicator
#' @param year This specifies the last year of your time series, which is used for creating a legend for the function
#'
#'
#' @return
#' This function returns a ggplot object
#'
#' @examples
#' 
#' \dontrun{
#' ## If ad0_shape_reg_df is a fortified data frame over a specific region, and centroids are the centroids for the defined
#' ## countries in that region labeled by country name.
#' gg_lastyear_map <- last_year_map(ad0_shape_reg_df, centroids, admin = 0)
#' }
#' @export
last_year_map <- function(shape, centroids, admin = 0, val_range = c(0, 1), distiller_direction = 1, year = 2015) {
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
        group = group, fill = mean
      )
    ) +
    geom_path_quiet(
      data = shape,
      aes(x = long, y = lat, group = group)
    ) +
    coord_equal() +
    theme_empty +
    labs(fill = paste0("Mean\nestimate", "\n(", year, ")"))

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
    ) +
      scale_fill_distiller(
        palette = "RdYlBu",
        limits = val_range,
        direction = distiller_direction
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
    ) +
      scale_fill_distiller(
        palette = "RdYlBu",
        limits = val_range,
        direction = distiller_direction
      )
  }
  return(gg)
}
