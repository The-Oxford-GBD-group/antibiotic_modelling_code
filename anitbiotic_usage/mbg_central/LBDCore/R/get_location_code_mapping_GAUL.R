#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param remove_diacritics PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[DBI]{dbDisconnect}}
#' @rdname get_location_code_mapping_GAUL
#' @export
#' @importFrom DBI dbDisconnect
get_location_code_mapping_GAUL <- function(remove_diacritics) {
  conn <- get_shared_db_conn()

  # Retrieve core fields and path_to_top_parent (used to calculate ihme_lc_id)
  all_locations_sql <- paste(
    "SELECT",
    "location_id         AS loc_id,",
    "location_name       AS loc_name,",
    "location_name_short AS loc_nm_sh,",
    "path_to_top_parent  AS path_to_top_parent",
    "FROM shared.location"
  )

  locs <- run_sql_query(conn, all_locations_sql)


  # Get GAUL_CODE metadata
  gaul_code_sql <- paste(
    "SELECT",
    "location_id             AS loc_id,",
    "location_metadata_value AS GAUL_CODE",
    "FROM shared.location_metadata_history",
    # id 26 is GAUL codes
    "WHERE location_metadata_type_id = 26",
    # 19 is 2017.g - the latest version of the metadata before
    # the location_metadata table was blown away ~25 June
    "  AND location_metadata_version_id = 19"
  )
  gaul_codes <- run_sql_query(conn, gaul_code_sql)
  gaul_codes["GAUL_CODE"] <- as.numeric(gaul_codes$GAUL_CODE)


  # Get ISO3 names (part of ihme_lc_id)
  iso_lookup_sql <- paste(
    "SELECT",
    "location_id             AS loc_id,",
    "location_metadata_value AS iso_name",
    "FROM shared.location_metadata_history",
    # id 1 is "short identifier"/ISO3 code
    "WHERE location_metadata_type_id = 1",
    "  AND location_metadata_version_id = 19"
  )
  iso_lookup <- run_sql_query(conn, iso_lookup_sql)


  data <- merge(locs, gaul_codes, by = "loc_id")
  data["ihme_lc_id"] <- sapply(data$path_to_top_parent, function(path_str) {
    get_ihme_lc_id(iso_lookup, path_str)
  })

  # remove path_to_top_parent
  data <- subset(data, select = c(
    "loc_id",
    "loc_name",
    "loc_nm_sh",
    "ihme_lc_id",
    "GAUL_CODE"
  ))
  data <- data.table(data)

  DBI::dbDisconnect(conn)

  # Fix diacritics
  if (remove_diacritics) {
    data$loc_name <- fix_diacritics(data$loc_name)
    data$loc_nm_sh <- fix_diacritics(data$loc_nm_sh)
  }

  # cast to data.table for usability/backwards compatibility
  return(data)
}
