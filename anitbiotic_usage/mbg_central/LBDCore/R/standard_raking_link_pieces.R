#' @title Assemble and return metadata for LBD standard raking.
#'
#' @description Assembles metadata from the LBD standard raking shapefile for
#' use in building raster layers based upon pixel coverage and not centroid
#' ownership.
#'
#' @param shapefile_version Shapefile version. Default: 'current'
#' @param countries_not_to_subnat_rake as it sounds. Replaces subnational metadata with national metadata. Default: NULL
#'
#' @return A list with admin_codes and code_locid
#' \item{admin_codes}{A named list with keys for all administrative levels
#' we rake to, likely "ADM0_CODE" and "ADM1_CODE". The values associated are
#' vectors containing all the admin codes we rake to.}
#' \item{code_locid}{A data.table with two columns: admin_code and loc_id.
#' This is a simplification of the loc_id, ADM0_CODE, and ADM1_CODE fields.
#' E.g., for geographies raked to admin 0 the admin_code column will contain
#' relevant ADM0_CODE corresponding to loc_id values.}
#'
#' @export
standard_raking_link_pieces <- function(shapefile_version = "current", countries_not_to_subnat_rake = NULL) {
  raking_dbf_path <- get_admin_shapefile(type = "raking", suffix = ".dbf", version = shapefile_version)
  raking_records <- data.table(foreign::read.dbf(raking_dbf_path))

  # replace metadata for countries that are specified as not subnational
  if (!is.null(countries_not_to_subnat_rake)) {
    subnat_change_codes <- get_adm0_codes(countries_not_to_subnat_rake, shapefile_version = shapefile_version)
    raking_records[ADM0_CODE %in% subnat_change_codes, `:=`(ad_level = 0, ADM1_CODE = 0, ADM1_NAME = NA)]

    code_replacement <- get_gbd_locs("all", rake_subnational = F)

    for (code in subnat_change_codes) {
      raking_records[ADM0_CODE %in% code, loc_id := code_replacement[ADM_CODE == code]$location_id]
    }
    raking_records <- unique(raking_records)
  }

  # key by "ADM0_CODE" or "ADM1_CODE"; returns those codes we rake to
  admin_codes <- list()
  # used to hold sub-data.table values for each admin level we rake to.
  code_locid_pieces <- list()
  i <- 1

  for (level in unique(raking_records$ad_level)) {
    field <- paste0("ADM", level, "_CODE")
    records_at_level <- raking_records[raking_records$ad_level == level, ]

    raking_targets <- records_at_level[[field]]
    admin_codes[[field]] <- raking_targets

    code_locid_pieces[[i]] <- data.table(
      admin_code = raking_targets,
      loc_id = records_at_level$loc_id
    )
    i <- i + 1
  }
  result <- list(
    admin_codes = admin_codes,
    code_locid = do.call(rbind, code_locid_pieces)
  )
  return(result)
}
