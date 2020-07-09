#' @title Apply raking factors
#' @description A function to apply raking factors to a cell pred
#'
#' @param cell_pred Matrix. Cell_pred object
#' @param simple_raster Raster. Should be the rasterized version of a shapefile or otherwise denoting the gaul/admin codes specified in rake_dt
#' @param rake_dt data.table Data.table output of calculate_raking_factors. Or at the very least, a three column data.table with columns for loc, year and raking_factor.
#' @param rake_method character string. Determines whether logit raking or linear scaling occurs
#' @param force_simp_ras_dt_match logical. Determines whether the function should break if it detects a difference between number of country years models and number of country years
#'                                         supplied to rake.
#' @return \code{cell_pred}
#' @export
apply_raking_factors <- function(cell_pred,
                                 simple_raster,
                                 rake_dt,
                                 rake_method,
                                 force_simp_ras_dt_match = T) {
  cpdim <- dim(cell_pred)

  # check to make sure simple raster and cell_pred play nice
  nyears <- dim(cell_pred)[1] %% length(cellIdx(simple_raster)) != 0
  if (nyears) {
    stop("good cells in simple raster is not a multiple of cell pred rows")
  }
  nyears <- dim(cell_pred)[1] / length(cellIdx(simple_raster))
  thelocs <- unique(as.vector(simple_raster))
  thelocs <- thelocs[!is.na(thelocs)]

  if (force_simp_ras_dt_match) {
    if (nrow(rake_dt) != (length(thelocs) * nyears)) {
      stop("Combination of inputs suggest a disconnect between rake_dt and cell pred")
    }
  }

  # create comparison to cell pred to fill in the raking factors
  dt <- rbindlist(lapply(unique(rake_dt[, year]), function(x) data.table(
      cell_xy_id = cellIdx(simple_raster),
      loc = simple_raster[][cellIdx(simple_raster)],
      year = x
    )))
  dt[, id := .I]

  # merge on the raking factors
  rake_dt <- merge(dt, rake_dt, all.x = T, by = c("loc", "year"))

  # enfore prespecified order
  setorder(rake_dt, id)

  if (rake_method == "linear") {
    cell_pred <- cell_pred * rake_dt[, raking_factor]
  } else {
    cell_pred <- invlogit(logit(cell_pred) + rake_dt[, raking_factor])
  }

  return(cell_pred)
}
