#' @title Generate location ids
#'
#' @description Generate \code{ihme_lc_id} for a national or subnational location
#'
#' @param iso_lookup a data.table with loc_id and iso_name columns.
#' @param loc_id_hierarchy a comma-delimited string of loc_id values
#'   representing the logical path from EARTH through each admin unit to the
#'   desired location.
#' @return String representing the location id.
#' @export
get_ihme_lc_id <- function(iso_lookup, loc_id_hierarchy) {
  path <- lapply(strsplit(loc_id_hierarchy, ","), as.numeric)[[1]]
  # first value is always EARTH id; second value is the country id
  nation_id <- path[2]
  nation_name <- iso_lookup[iso_lookup$loc_id == nation_id, "iso_name"]
  # Nations have a location code of their ISO name
  if (length(path) == 2) {
    return(nation_name)
  }
  # Subnationals have a location code of ISO_LOCATIONID
  id <- sprintf("%s_%i", nation_name, path[length(path)])
  # Special case - Puerto Rico
  if (id == "USA_385") {
    id <- "PRI"
  }
  return(id)
}
