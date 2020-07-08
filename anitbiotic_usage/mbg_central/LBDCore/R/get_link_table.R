#' @title Subset global link table to regional table
#'
#' @description Reads in the global link table and id raster and subsets using
#' a regional simple raster.
#'
#' @param simple_raster Simple raster used for modelling region
#'
#' @return list of 2 objects:
#'    - a link table
#'    - a vector of pixel ids used to link one year in a cell pred to the link table.
#'
#' @importFrom raster crop mask extract
#' @export
get_link_table <- function(simple_raster, shapefile_version) {
  # read in link_table
  global_link <- data.table(readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_link.rds", shapefile_version)))
  # read in id_raster
  global_raster <- readRDS(sprintf("/snfs1/WORK/11_geospatial/admin_shapefiles/%s/lbd_standard_id_raster.rds", shapefile_version))
  # crop id_raster to simple_raster
  cropped_raster <- raster::crop(global_raster, simple_raster)
  # mask id_raster with simple_raster
  masked_raster <-raster::mask(cropped_raster, simple_raster)
  # extract id_raster
  pixel_ids <- raster::extract(masked_raster, extent(masked_raster), na.rm = T)
  pixel_ids <- pixel_ids[!is.na(pixel_ids)]
  # subset link table by pixel ids from extract
  link_table <- global_link[ID %in% pixel_ids, ]
  # make sure list of pixel ids is same number of nonna cells in simple raster
  if (length(pixel_ids) != length(which(!is.na(getValues(simple_raster))))) {
    stop("The number of pixel_ids does not match the number of non-na values in simple raster. \nPart of the simple raster may be outside the extent of the global id raster.")
  }
  # return subsetted link table and list of pixel ids
  return(list("link_table" = link_table, "pixel_ids" = pixel_ids))
}
