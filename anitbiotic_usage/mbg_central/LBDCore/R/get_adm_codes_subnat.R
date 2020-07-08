#' @title Get subnational administrative codes (adm1 and adm2)
#'
#' @description Pulls subnational administrative codes worldwide for a given
#'   administrative level (1 or 2) and a given set of country-level geographies,
#'   represented by a vector of standard Admin0 codes pulled using
#'   get_adm0_codes(). This function generalizes get_gaul_codes_subnat(), which
#'   will be deprecated.
#'
#' @param adm0_list A numeric vector of Admin0 codes pulled using
#'   get_adm0_codes().
#' @param admin_level A subnational administrative level: 1 and 2 are currently
#'   supported.
#' @param shapefile_version A character string indicating which shapefile version to pull
#'
#' @return list of administrative codes
#' @export
get_adm_codes_subnat <- function(adm0_list, admin_level, shapefile_version = "current") {
  # Read the table data for the current administrative shapefile
  hierarchy <- read.dbf(get_admin_shapefile(admin_level,
    suffix = ".dbf",
    version = shapefile_version
  ))
  hierarchy <- as.data.table(hierarchy)
  # Rename the admin0 field to ADM0_CODE
  adm0_name <- grep("ADM0_CODE", names(hierarchy), value = T)
  names(hierarchy)[names(hierarchy) == adm0_name] <- "ADM0_CODE"
  # Keep only rows within the given Admin0s, and keep only the column for the
  #  relevant administrative level codes
  hierarchy <- hierarchy[ADM0_CODE %in% adm0_list, ]
  adm_specific_name <- grep(
    paste0("ADM", admin_level, "_CODE"), names(hierarchy),
    value = T
  )
  adm_list <- unique(hierarchy[[adm_specific_name]])
  return(adm_list)
}
