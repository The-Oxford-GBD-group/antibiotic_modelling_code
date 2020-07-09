#' @title Get GAUL Codes (TO BE DEPRECATED)
#'
#' @description Pull Admin0 codes for the specified countries or regions,
#'   optionally excluding certain Admin0 codes. The only difference from the
#'   standard get_adm0_codes() function is that the default adm0_type is 'gaul'
#'   rather than 'gadm'.
#'
#' @note THIS FUNCTION IS BEING DEPRECATED - ALWAYS USE get_adm0_codes() INSTEAD.
#'
#' @export
get_gaul_codes <- function(
                           adm0s,
                           strict = FALSE,
                           lookup_table = NULL,
                           core_repo = NULL,
                           adm0_type = "gaul",
                           shapefile_version = NULL) {
  get_adm0_codes(
    adm0s = adm0s,
    strict = strict,
    lookup_table = lookup_table,
    core_repo = core_repo,
    adm0_type = adm0_type,
    shapefile_version = shapefile_version
  )
}
