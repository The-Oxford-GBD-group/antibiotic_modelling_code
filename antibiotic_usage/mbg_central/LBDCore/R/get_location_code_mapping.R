#' @title Return location metadata
#'
#' @param shapefile_version string indicating version of shapefile to use. 'gaul' or 'gadm' is inferred from this
#' @param fix_diacritics (default TRUE) whether to strip all diacritic characters from result.
#'
#' @description Get location metadata for:
#'  IHME location ids (loc_id),
#'  Admin codes (ADM_CODE),
#'  IHME loc ids (ihme_lc_id),
#'  Long location names (loc_name),
#'  Short location names (loc_nm_sh)
#'
#' @note: includes a \code{GAUL_CODE} column (same values as \code{ADM_CODE}) for compatibility.
#'
#' @return data.frame with above columns
#' @export
get_location_code_mapping <- function(shapefile_version, remove_diacritics = T) {
  shapefile_type <- detect_adm_shapefile_date_type(shpfile_path = get_admin_shapefile(version = shapefile_version))$shpfile_type

  if (shapefile_type == "gaul") {
    data <- get_location_code_mapping_GAUL(remove_diacritics = remove_diacritics)
    data[["ADM_CODE"]] <- data[["GAUL_CODE"]]
    return(data)
  } else if (shapefile_type == "gadm") {
    data <- fread("/snfs1/WORK/11_geospatial/10_mbg/lbd-gbd-location-metadata.csv")
    data[["GAUL_CODE"]] <- data[["ADM_CODE"]]

    if (remove_diacritics) {
      data$loc_name <- fix_diacritics(data$loc_name)
      data$loc_nm_sh <- fix_diacritics(data$loc_nm_sh)
    }

    return(data)
  } else {
    stop(paste0("Must provide gaul or gadm as shapefile_type, not ", shapefile_type))
  }
}
