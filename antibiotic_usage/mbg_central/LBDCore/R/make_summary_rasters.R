#' @title Makes summary rasters from cell pred
#'
#' @description wrapper function for make_cell_pred_summary
#'
#' @param cell_pred cell pred object
#' @param simple_raster admin raster matching the cell pred object
#' @param summary_measures charater vector - functions to summarize by. see default below
#' @param raked boolean default `T` - adds "raked" or "unraked" to name of output
#'
#' @return a named list with a raster brick for each summary measure
#'
#' @export
make_summary_rasters <- function(cell_pred,
                                 simple_raster,
                                 summary_measures = c("mean", "cirange", "lower", "upper"),
                                 raked = T) {
  message("\nMaking summary rasters from cell pred")
  outputlist <-
    lapply(summary_measures, function(x) {
      message("   ", x, ":")
      make_cell_pred_summary(
        draw_level_cell_pred = cell_pred,
        mask = simple_raster,
        return_as_raster = TRUE,
        summary_stat = x
      )
    })
  names(outputlist) <- paste0(summary_measures, ifelse(raked, "_raked", "_unraked"), "_raster")

  message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  message("           Summary Rasters Complete")
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  return(outputlist)
}
