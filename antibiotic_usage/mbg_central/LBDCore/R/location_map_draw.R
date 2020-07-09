#' @title Create a locator map that highlights the admin level in question in red within a map background
#'
#' @description This function is called in the subnational_ts_function and is a way to generalize locator maps
#' that are useful to contextualize where the admin unit lies within a reigon or country.
#'
#' @author Michael Cork, \email{mcork23@uw.edu}
#'
#' @param admin_shape SpatialPolygonsDataFrame specified to the specific admin unit, colored in red
#' @param surround_shape SpatialPolygonsDataFrame of surrounding geography to place admin unit.
#' If highlighted admin_shape is a country, then the surround_shape is typically the region it falls in
#'
#' @return
#' This function returns a ggplot object
#'
#' @examples
#' \dontrun{
#' ## ad0_shape_simple is the simplified ad0 shapefile for Africa, and ad0_reg_codes defines the
#' ## admin 0 codes for a specified region in Africa
#' admin_shape <- subset(ad0_shape_simple, ADM0_CODE %in% ad0_reg_codes)
#' surround_shape <- ad0_shape_simple
#' location_map <- location_map_draw(admin_shape, surround_shape)
#' }
#' @export
location_map_draw <- function(admin_shape, surround_shape) {
  gg_location <-
    ggplot() +
    geom_polygon_quiet(
      data = admin_shape,
      aes(x = long, y = lat, group = group),
      fill = "red"
    ) +
    geom_path_quiet(
      data = surround_shape,
      aes(x = long, y = lat, group = group),
      size = 0.05,
      alpha = 0.5
    ) +
    geom_path_quiet(
      data = surround_shape,
      aes(x = long, y = lat, group = group),
      size = 0.3
    ) +
    coord_equal() +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  # gg_location <- aspect_ratio_plot(gg_location, 4/3, 0.25)
  return(gg_location)
}
