#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rbrick PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_gg_map
#' @export
make_gg_map <- function(rbrick, title, i) {
  r_df <- rbrick[[i]] %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
  names(r_df) <- c("value", "x", "y")
  gg_result <- ggplot() +
    geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
    coord_equal() +
    theme_empty() +
    labs(title = title) +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = ifelse(highisbad, -1, 1),
      limits = c(zmin, zmax)
    ) +
    geom_path(
      data = master_shape_df,
      aes(x = long, y = lat, group = group)
    )
  return(gg_result)
}
