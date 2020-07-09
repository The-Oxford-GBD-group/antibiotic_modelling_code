#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION, Default: data
#' @param strata PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_strat_rows
#' @export
get_strat_rows <- function(data = data,
                           strata,
                           ...) {

  ## this function returns all the rows in a strata

  if (length(strata) < 1) {
    message("Need to identify some strata!")
    stop()
  }

  ## loop through and intersect all rows we want
  good_rows <- data[, colnames(strata)[1] ] == strata[[1]]

  if (length(strata) > 1) {
    for (c in 2:ncol(strata)) {
      tmp_rows <- data[, colnames(strata)[c] ] == strata[[c]]
      good_rows <- good_rows * tmp_rows ## intersect them
    }
  }

  good_rows <- which(good_rows == 1)

  return(good_rows)
}
