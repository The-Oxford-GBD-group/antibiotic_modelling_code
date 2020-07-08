#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param shp_path PARAM_DESCRIPTION, Default: '/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory'
#' @param ignore_warnings PARAM_DESCRIPTION, Default: TRUE
#' @param cores PARAM_DESCRIPTION, Default: as.numeric(slots)
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param unique_vars PARAM_DESCRIPTION, Default: NULL
#' @param density PARAM_DESCRIPTION, Default: 0.001
#' @param perpixel PARAM_DESCRIPTION, Default: TRUE
#' @param prob PARAM_DESCRIPTION, Default: TRUE
#' @param use_1k_popraster PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname resample_polygons_dev
#' @export
resample_polygons_dev <- function(data,
                                  shp_path = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory",
                                  ignore_warnings = TRUE,
                                  cores = as.numeric(slots),
                                  indic = indicator,
                                  unique_vars = NULL,
                                  density = 0.001,
                                  perpixel = TRUE,
                                  prob = TRUE,
                                  use_1k_popraster = TRUE) {
  warning("resample_polygons_dev() is deprecated. Use resample_polygons() instead")
  message("Running resample_polygons() with pull_poly_method = \"mclapply\"")
  rp <- resample_polygons(
    data = data,
    shp_path = shp_path,
    ignore_warnings = ignore_warnings,
    cores = cores,
    indic = indic,
    unique_vars = unique_vars,
    density = density,
    perpixel = perpixel,
    prob = prob,
    use_1k_popraster = use_1k_popraster,
    pull_poly_method = "mclapply"
  )
  return(rp)
}
