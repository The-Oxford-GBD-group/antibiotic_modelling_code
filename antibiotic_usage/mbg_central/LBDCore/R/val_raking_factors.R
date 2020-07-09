#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rfs PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @param gaul_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname val_raking_factors
#' @export
val_raking_factors <- function(rfs,
                               shapefile_version = "current",
                               gaul_list) {
  rfs <- rfs[name %in% gaul_list, ]
  loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)
  setnames(rfs, "name", "GAUL_CODE")
  rfs <- merge(rfs, loc_names, by = "GAUL_CODE")
  gg_rfs <- ggplot(
    data = rfs,
    aes(
      x = rake_to_mean,
      y = geo_mean,
      color = year
    )
  ) +
    geom_text(label = rfs$year) +
    geom_line(aes(x = geo_mean, y = geo_mean), col = "black", size = 1) +
    ylab("MBG Mean") +
    xlab("GBD Mean") +
    theme_minimal() +
    theme(legend.position = "none")
  return(gg_rfs)
}
