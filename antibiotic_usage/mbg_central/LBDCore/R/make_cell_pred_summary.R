
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param draw_level_cell_pred PARAM_DESCRIPTION
#' @param mask PARAM_DESCRIPTION, Default: simple_raster
#' @param return_as_raster PARAM_DESCRIPTION, Default: TRUE
#' @param summary_stat PARAM_DESCRIPTION, Default: 'mean'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_cell_pred_summary
#' @export
make_cell_pred_summary <- function(draw_level_cell_pred,
                                   mask = simple_raster,
                                   return_as_raster = TRUE,
                                   summary_stat = "mean",
                                   ...) {

  #################################################################################
  ### Takes in raked or raw draw-level estimates and makes stat summary rasters
  ## Inputs:
  # draw_level_cell_pred: Cells by Draws matrix which is output from predict_mbg() or from rake_predictions()
  # mask: Should be the simple_raster
  # return_as_raster: If TRUE returns as raster, else as table
  # summary_stat: ie mean, cirange, quantile, sd
  ## Outputs: Summary table or raster of the cell_pred table put in
  #################################################################################

  # make summary
  summ <- apply(draw_level_cell_pred, 1, summary_stat, ...)

  # put it in a raster
  if (return_as_raster) {
    yrs <- dim(draw_level_cell_pred)[1] / length(cellIdx(mask))
    message(sprintf("Making a RasterBrick with %i layers", yrs))
    summ <- insertRaster(mask, matrix(summ, ncol = yrs))
  }


  return(summ)
}
