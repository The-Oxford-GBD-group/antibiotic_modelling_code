#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param regions PARAM_DESCRIPTION
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
#' @rdname make_regions_map
#' @export
make_regions_map <- function(regions, subset_shape, shapefile_version = "current") {
  col_list <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999")
  i <- 1
  plot(subset_shape)
  for (reg in regions) {
    shapes <- subset_shape[subset_shape$GAUL_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version), ]
    plot(shapes, add = TRUE, col = col_list[i])
    i <- i + 1
  }
}
