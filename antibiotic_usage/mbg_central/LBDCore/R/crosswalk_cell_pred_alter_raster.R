#' @title Crosswalk Cell Pred Object
#'
#' @description This version of the crosswalk edits the new simple raster to match the cell pred by making any
#' pixel not in the original simple raster na. Nonreversible.
#'
#' @param simple_raster Simple raster used to create the cell pred object (created by build_simple_raster_pop).
#' @param new_simple_raster Simple raster to be crosswalked to (created by build_simple_raster_pop).
#' @param cell_pred Cell pred object that matches with simple raster.
#' @param year_list List of years
#'
#' @return Returns crosswalked cell pred object and altered new simple raster
#' @export
#'
#' @examples
#' \dontrun{
#' new_pred_object <- crosswalk_cell_pred(simple_raster, new_simple_raster, cell_pred, years)
#' }
#' @note This function loops through each pixel in the rasters and compares them. pixels where both are NA or both have values are kept the same. Pixels where the original raster has a value and the new is na have the row in the cell pred object deleted. For pixels where the original is na and the new has a value, that pixel in the new raster is changed to an NA because there is no corresponding value in the cell pred object for that pixel.
#'
crosswalk_cell_pred_alter_raster <- function(simple_raster, new_simple_raster, cell_pred, year_list) {
  # number of years in year list
  years <- length(year_list)
  # get a list with number of rows equal to one year of cell pred object
  rows <- c(1:(nrow(cell_pred) / years))
  # initialize vector to track row indices to drop
  rows_to_drop <- c()
  # initialize to count times where pixel in new raster is set to NA
  new_pixels_set_NA <- 0
  # intitialize to track cell_pred index
  cell_pred_pointer <- 1
  # convert rasters to list for speed purposes
  simple_raster_list <- raster::extract(simple_raster, extent(simple_raster))
  new_simple_raster_list <- raster::extract(new_simple_raster, extent(new_simple_raster))
  # loop through raster and compare values for each pixel
  for (i in 1:length(simple_raster_list)) {
    # if both rasters are NA, do nothing
    if (is.na(simple_raster_list[i]) & is.na(new_simple_raster_list[i])) {
      next
      # if both rasters are not NA, advance cell_pred_pointer
    } else if (!is.na(simple_raster_list[i]) & !is.na(new_simple_raster_list[i])) {
      cell_pred_pointer <- cell_pred_pointer + 1
      # if the original raster is not na and the new raster is na, add row index for deletion from cell pred
    } else if (!is.na(simple_raster_list[i]) & is.na(new_simple_raster_list[i])) {
      rows_to_drop <- c(rows_to_drop, cell_pred_pointer)
      cell_pred_pointer <- cell_pred_pointer + 1
      # if the original raster is na and the new raster is not na, set pixel in new raster to NA
    } else if (is.na(simple_raster_list[i]) & !is.na(new_simple_raster_list[i])) {
      new_simple_raster[i] <- NA
      new_pixels_set_NA <- new_pixels_set_NA + 1
    }
  }
  # get boolean vector for dropping
  drop_logical <- rows %in% rows_to_drop
  # duplicate vector for each year in the
  drop_logical <- rep(drop_logical, times = years)

  # drop rows in cell pred
  cell_pred <- cell_pred[!drop_logical, ]

  # return new cell_pred and simple_raster
  return(list(cell_pred, new_simple_raster))
}
