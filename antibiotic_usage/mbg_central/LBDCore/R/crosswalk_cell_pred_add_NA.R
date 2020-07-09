#'  @title Crosswalk Cell Pred Object
#'
#' @description This version of the crosswalk adds NAs to the cell pred where there are pixels in the new simple
#' raster but not in the old. Reversible.
#'
#' @param simple_raster Simple raster used to create the cell pred object (created by build_simple_raster_pop).
#' @param new_simple_raster Simple raster to be crosswalked to (created by build_simple_raster_pop).
#' @param cell_pred Cell pred object that matches with simple raster.
#' @param year_list List of years
#'
#' @return Returns crosswalked cell pred object
#' @export
#'
#' @examples
#' \dontrun{
#' new_pred_object <- crosswalk_cell_pred(simple_raster, new_simple_raster, cell_pred, years)
#' }
#' @note This function loops through each pixel in the rasters and compares them. pixels where both are NA or both have values are kept the same. Pixels where the original raster has a value and the new is na have the row in the cell pred object deleted. For pixels where the original is na and the new has a value, an NA row is added to the cell pred object.
crosswalk_cell_pred_add_NA <- function(simple_raster, new_simple_raster, cell_pred, year_list) {
  # number of years in year list
  years <- length(year_list)
  # get a list with number of rows equal to one year of cell pred object
  rows <- c(1:(nrow(cell_pred) / years))
  # initialize vector to track row indices to drop
  rows_to_drop <- c()
  # initialize vector to track row ids to add
  rows_to_add <- c()
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
      rows_to_add <- c(rows_to_add, cell_pred_pointer)
    }
  }
  # get boolean vector for dropping
  drop_logical <- rows %in% rows_to_drop
  # duplicate vector for each year in the
  drop_logical <- rep(drop_logical, times = years)

  # add id column to cell_pred
  rows_to_add_id <- 1:nrow(cell_pred)
  cell_pred <- cbind(rows_to_add_id, cell_pred)

  # drop rows in cell pred
  cell_pred <- cell_pred[!drop_logical, ]

  if (length(rows_to_add) > 0) {
    # calculate row positions for new rows to be added in
    rows_to_add <- rows_to_add - 1 + 0.00001
    if (length(rows_to_add) == 1) {
      rows_to_add[1] <- rows_to_add[1] + 0.00001
    } else {
      for (i in 2:length(rows_to_add)) {
        while (rows_to_add[i] <= rows_to_add[i - 1]) {
          rows_to_add[i] <- rows_to_add[i] + 0.00001
        }
      }
    }

    # duplicate row positions for multiple years
    dup <- rows_to_add
    for (i in 2:years) {
      new <- dup + (length(rows) * (i - 1))
      rows_to_add <- c(rows_to_add, new)
    }

    # construct NA matrix
    add_matrix <- matrix(nrow = length(rows_to_add), ncol = (ncol(cell_pred) - 1))
    add_matrix <- cbind(rows_to_add, add_matrix)

    # add NA matrix to cell pred, sort by row id and drop row id column
    cell_pred <- rbind(cell_pred, add_matrix)
    cell_pred <- cell_pred[order(cell_pred[, 1]), ]
  }

  cell_pred <- cell_pred[, 2:ncol(cell_pred)]
  return(cell_pred)
}
