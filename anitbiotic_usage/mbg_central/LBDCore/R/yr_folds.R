#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param yr PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param n_folds PARAM_DESCRIPTION, Default: 5
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname yr_folds
#' @export
yr_folds <- function(xy, ## xy location matrix
                     yr, ## yr vec
                     ss = 1, ## sample size vec (or 1 if equi-ss)
                     n_folds = 5,
                     ...) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## 3) full years, randomly selected across the duration of data
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  if (length(unique(yr) < n_folds)) {
    message("Too many folds for too few years! Try again in a decade")
    stop()
  }

  if (length(ss) == 1) ss <- rep(1, nrow(xy))

  ## first we find the sample size in each of the countries
  dt <- data.table(
    long = xy[, 1],
    lat = xy[, 2],
    ss = ss,
    yr = yr
  )

  ## get sample size totals in each country
  cts_in_yr <- dt[, sum(ss), by = ct]

  ## make the folds
  fold_vec <- make_folds_by_poly(
    cts_in_polys = as.data.frame(cts_in_yr),
    pt_poly_map = as.character(dt[, ct]),
    n_folds = n_folds
  )

  fold_list <- list(NULL)
  for (i in sort(unique(fold_vec))) {
    fold_list[[i]] <- which(fold_vec == i)
  }
  return(fold_list)
}
