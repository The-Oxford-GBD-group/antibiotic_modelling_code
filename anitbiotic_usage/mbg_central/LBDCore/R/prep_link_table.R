#' @title Prep Link Table
#' @description This prepares the link table for merging and does the zippering operation on the region boundary.
#' The zippering means that a given cell is only in one modeled reagion.
#'
#' @param link_table the link table for your region
#' @param simple_raster the simple raster for your region
#' @param pixel_ids a list of all valif pixel ids in your simple raster
#'
#' @return a link table such that cell fractions that fall either in the ocean or in a country outside of your region are dropped
#' and the areas of the remain cell fractions for those cells are rescaled to equal 1 so no data is lost.  The result is cells that
#' lie on the border of the region are zippered into one region or the other.
#' @export
#'
prep_link_table <- function(link_table = link_table,
                            simple_raster = simple_raster,
                            pixel_id = pixel_id) {
  #####################################################################
  # Prepping the cell_pred and link table to be linked


  # keep only locations in the region
  link <- link_table[[1]]
  link <- link[ADM0_CODE %in% unique(simple_raster), ] # This means only cells and cell fractions that are in the region are included in the link table

  # keep only cells where simple raster identifies it as belonging to the country or region
  link_map <- data.table(af_id = link_table[[2]], reg_id = pixel_id, simp_ras_val = simple_raster[pixel_id]) # creates table relating pixel number in the africa simple raster (id in link table) to pixel number in simple raster (id in simple raster/cell pred), to the value in the simple raster(ADM0_CODE).
  link <- merge(link, link_map, by.x = "ID", by.y = "af_id") # merges that relational table to the link table

  # scale up such that the sum of area fractions for each pixel is equal to 1 so pieces are not lost
  link[, total_frac := sum(area_fraction), by = "ID"]
  link[, c("area_fraction", "old_frac") := list(area_fraction / total_frac, area_fraction)]

  return(link)
}
