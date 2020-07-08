#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param simple_raster PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname add_gauls_regions
#' @export
#' @importFrom raster extract
add_gauls_regions <- function(df, simple_raster, shapefile_version = "current") {

  # Extract GAUL_CODE from simple_raster using lat/longs
  df$ADM0_CODE <- raster::extract(simple_raster, df[, c("longitude", "latitude"), with = F])

  # Add names of regions by GAUL_CODE
  for (r in Regions) {
    df <- df[ADM0_CODE %in% get_adm0_codes(r, shapefile_version = shapefile_version), region := r]
  }

  ## Check that merge of GAUL_CODE worked properly
  df <- df[!is.na(region), good_records := 1]
  message(paste0(length(df$good_records) - length(df[good_records == 1, N]), " out of ", length(df$good_records), " could not have GAUL/region extracted properly. Probably coastal points, need to fix."))
  df <- df[good_records == 1, ]
}
