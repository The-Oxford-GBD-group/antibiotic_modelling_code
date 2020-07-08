#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param d PARAM_DESCRIPTION
#' @param re PARAM_DESCRIPTION, Default: Regions
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname merge_with_ihme_loc
#' @export
merge_with_ihme_loc <- function(d, re = Regions, shapefile_version = "current") {
  message(nrow(d))
  gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
  d <- d[, ihme_lc_id := as.character(country)]

  d <- merge(d, gaul_to_loc_id, by = "ihme_lc_id", all.x = T)

  message(nrow(d))
  for (r in re) {
    d <- d[GAUL_CODE %in% get_adm0_codes(r, shapefile_version = shapefile_version), region := r]
  }
  return(d)
}
