#' @title Calculate Fractional Raking Factors
#'
#' @description Calculate raking factors considering cases where a pixel is in
#' multiple admin units for deaths or cases
#'
#' @param cell_pred a cell pred with deaths/cases in data.table format with a pixel_id column from `get_link_table()`
#' @param link link table gotten from `get_link_table()`
#' @param gbd data.table with a columns - `gaul`, `year_id`, and `gbd_field (see below)`
#' with estimates for the indicator being raked to for each year-admin0
#' @param region the region used to make the simple raster
#' @param overs a vector of the column names in the cell pred corresponding to draws
#' (typically V1:Vndraws)
#' @param shapefile_version string indicating which shapefile version to match ADM0 codes against
#'
#' @return a \code{data.table} with raking factor by adm0 gaul code and year
#'
#' @export
#'
#' @details
#' In using 5x5 pixels, some pixels are split between several admin
#' units. Fractional raking is a way of raking counts which takes
#' this into consideration. For any pixel that is split between
#' admin units, the fractional area of that admin unit within the
#' pixel is multiplied by the count so that counts are properly
#' attributed to their respective admin units.
#'
#' The key to fractional raking is a [link table] which has a row
#' for each pixel-admin unit, and includes the gaul codes for admins
#' 0-2, as well as the fractional area of the admin unit in that pixel
#' (a pixel split between two admin 2 units would have two rows in the
#' link table). There is a global link table generated from the stage 1
#' and stage 2 simple raster, where every pixel gets a unique id.
#' Practically, when using a link table, the global link table and raster
#' are subsetted based on the simple raster of the region being raked
#' using get_link_table()
#'
#' Build_link_polygon() and build_link_table() are functions used to
#' create the link table, which will only need to be used when
#' there is a change in the shapefile used to generate simple rasters.
#'
#' Many of the functions require the parameter overs, which is a vector
#' of the column names of the draws, typically named V1 to Vndraws
#' when the cell pred is converted to a data.frame or data.table.
#'
#'
#' For additional questions, ask Michael Collison
get_fractional_rf <- function(cell_pred, link, gbd, region, overs, shapefile_version) {

  # merge link table and cell pred on pixel id
  linked_cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "pixel_id", all.y = T, allow.cartesian = T)

  if (nrow(linked_cell_pred[is.na(ADM_CODE)]) > 0) {
    message("There were ", nrow(linked_cell_pred[is.na(ADM_CODE)]), " rows in the dataset that did not have a corresponding pixel in the link table")
    message("The following pixels (pixel_id from link table) were affected: ")
    message(paste(linked_cell_pred[is.na(ADM_CODE)]$pixel_id, collapse = ", "))
  }

  # multiply death draws by area fractions and aggregate draws to ADM0 and year
  linked_cell_pred <- linked_cell_pred[, lapply(overs, function(x) sum(get(x) * as.numeric(area_fraction), na.rm = T)), by = c("year", "ADM_CODE", "ADM0_CODE")]

  # get mean of draws
  linked_cell_pred[, mbg_rf := rowMeans(linked_cell_pred[, overs, with = F])]

  # merge with gbd and calculate population raking factor
  rf_df <- merge(linked_cell_pred, gbd, by = c("ADM_CODE", "year"), all.x = T)
  rf_df <- rf_df[, rf := mean / mbg_rf]

  # drop unecessary columns
  rf <- rf_df[, c("ADM_CODE", "year", "rf", "ADM0_CODE")]

  # set raking factors for countries not in region to 1 to avoid overraking in countries
  # partially included in the simple raster. Not doing this causes the values of border
  # pixels to be much higher than they should be.
  rf[!(ADM0_CODE %in% get_adm0_codes(region, shapefile_version = shapefile_version)), rf := 1]
  return(rf)
}
