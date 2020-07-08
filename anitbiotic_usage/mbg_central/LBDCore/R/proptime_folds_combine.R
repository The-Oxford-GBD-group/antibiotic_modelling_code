#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yr PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION
#' @param ts PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname proptime_folds_combine
#' @export
proptime_folds_combine <- function(yr, n_folds, ss, ts, ...) {

  # count up the sample size by year, and the number of unique observations by year
  totals <- data.table(
    year = sort(unique(yr)),
    total_n = tapply(ss, yr, sum),
    total_poly = tapply(ss, yr, length)
  )

  # starting with the first year, combine years until a minimum sample size and unique observations threshold is met.
  totals[, t_fold := as.numeric(NA)]
  fold <- 0

  while (sum(is.na(totals$t_fold)) > 0) {
    # iterate the fold number
    fold <- fold + 1

    # recalculate cumulative totals for all years that haven't been assigned a fold yet
    totals[is.na(t_fold), c("cum_total_n", "cum_total_poly") := list(cumsum(total_n), cumsum(total_poly))]

    # find the minimum year where the cumlative totals are sufficiently high
    yy <- totals[cum_total_n > (n_folds * ts * 2) & cum_total_poly > n_folds, ][1, year]

    # assign all years not already assigned and less than or equal to this year to a new fold
    if (!is.na(yy)) {
      totals[is.na(t_fold) & year <= yy, t_fold := fold]

      # if there is no unassigned year where we reach the threshold (ie, if we run out of time), combine remaining years into the previous fold
    } else {
      totals[is.na(t_fold), t_fold := fold - 1]
    }

    totals[, c("cum_total_n", "cum_total_poly") := NULL]
  }

  # create the folds based on this mapping
  fold_list <- lapply(1:max(totals$t_fold), function(fold) which(yr %in% totals[t_fold == fold, year]))
  return(fold_list)
}
