#' @title Get subnational GAUL codes
#'
#' @description Pulls subnational administrative codes worldwide for a given
#'   administrative level (1 or 2) and a given set of country-level geographies,
#'   represented by a vector of standard Admin0 codes pulled using
#'   get_adm0_codes(). This function is a simple wrapper for
#'   get_adm_codes_subnat() and has been kept for backwards-compatibility.
#'
#' @note THIS FUNCTION IS BEING DEPRECATED. ALWAYS USE \code{get_adm_codes_subnat()}
#'   INSTEAD.
#'
#' @export
get_gaul_codes_subnat <- function(gaul_list, admin_level, shapefile_version) {
  subnat_list <- get_adm_codes_subnat(
    adm0_list = gaul_list,
    admin_level = admin_level,
    shapefile_version = shapefile_version
  )
  return(subnat_list)
}
