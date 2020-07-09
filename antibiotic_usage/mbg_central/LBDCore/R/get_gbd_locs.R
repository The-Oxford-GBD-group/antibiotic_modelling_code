#' @title Get GBD locations
#' @description Loads in the shapefiles to get a complete set of codes for GBD
#' locations. this is particularly useful if you want to get
#' subnational GBD location codes which are a combination of different
#' administrative levels
#'
#' @param reg region you want to pull codes for. e.g. 'africa'
#' @param rake_subnational logical - do you want to pull location codes for subnational raking or just national raking
#' @param shapefile_version string specifying version date of shapefile to use
#'
#' @return returns a dataframe containing, at a minimum, an ADM_CODE column and an loc_id (ihme loc id) column. If rake_subnational=T, it will also contain ADM0*, ADM1* and rak_level (admin level for raking) columns
#' @export
get_gbd_locs <- function(reg,
                         rake_subnational = T,
                         shapefile_version = raking_shapefile_version) {
  if (rake_subnational == T) {
    connector <-
      st_read(get_admin_shapefile(admin_level = 0, raking = T, version = shapefile_version), quiet = T) %>%
      st_set_geometry(NULL) %>%
      mutate(ADM0_CODE = as.numeric(as.character(ADM0_CODE))) %>%
      mutate(ADM1_CODE = as.numeric(as.character(ADM1_CODE))) %>%
      filter(ADM0_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version, subnational_raking = TRUE))

    ## get the lowest raking level and add it onto connector
    if ("ad_level" %in% colnames(connector)) {
      connector$rak_level <- connector$ad_level ## gadm (ad_level) and gaul (rak_level) use different names for this!
    }

    connector <- connector %>%
      dplyr::select(ADM0_CODE, ADM1_CODE, loc_id, rak_level) %>%
      mutate(rak_level = as.character(rak_level)) %>%
      dplyr::rename(location_id = loc_id) %>%
      mutate(location_id = as.numeric(as.character(location_id)))

    ADM_CODE <- connector$ADM0_CODE
    for (rl in unique(connector$rak_level)) {
      ADM_CODE[which(rl == connector$rak_level)] <- connector[[paste0("ADM", rl, "_CODE")]][which(rl == connector$rak_level)]
    }

    connector <- cbind(connector, ADM_CODE)
  } else {
    connector <-
      get_location_code_mapping(shapefile_version = shapefile_version)[
        ADM_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version),
        list(location_id = loc_id, ADM_CODE)
      ]
  }
  return(data.table(connector))
}
