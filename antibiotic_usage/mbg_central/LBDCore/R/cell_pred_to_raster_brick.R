#' @title Convert cell pred into raster brick
#'
#' @description Insert cell pred object into a series of raster layers for each year
#' then combine into rasterBrick
#'
#' @param cell_pred a cell pred object in matrix format
#' @param simple_raster the simple raster corresponding to the cell_pred_object
#' @param year_list list of years in cell_pred
#' @param func function to summarize cell pred. `mean`, `upper`, and `lower` are acceptable
#'
#' @return a rasterBrick with a layer for each year in the cell_pred
#'
#' @export
cell_pred_to_raster_brick <- function(cell_pred, simple_raster, year_list, func) {
  raster_list <- vector("list", length = length(year_list))

  # apply summary function
  if (func == "mean") {
    message("summarizing draws to mean")
    death_vec <- rowMeans(cell_pred)
  } else if (func == "upper") {
    message("summarizing draws to upper quantile - this can be slow for large cell_preds")
    death_vec <- matrixStats::rowQuantiles(cell_pred, probs = 97.5 / 100)
  } else if (func == "lower") {
    message("summarizing draws to lower quantile - this can be slow for large cell_preds")
    death_vec <- matrixStats::rowQuantiles(cell_pred, probs = 2.5 / 100)
  } else {
    stop("Not a valid function, please choose 'mean', 'upper', or 'lower'.")
  }

  # split years into separate vectors in a list
  death_vec_list <- split(death_vec, cut(seq_along(death_vec), length(year_list), labels = FALSE))
  # get raster pixels with data
  pixel_id <- which(!is.na(getValues(simple_raster)))
  message("Building raster brick \n")
  # loop through year vectors and insert into rasters
  for (i in 1:length(death_vec_list)) {
    death_ras <- simple_raster
    raster_list[[i]] <- insertRaster(death_ras, cbind(death_vec_list[[i]]))
  }
  # set years as raster layer names
  names(raster_list) <- year_list

  # convert to rasterBrick
  death_raster <- brick(raster_list)
  return(death_raster)
}
