#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yr PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: NULL
#' @param n_folds PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname chrono_folds
#' @export
chrono_folds <- function(yr, ss = NULL, n_folds = NULL, ...) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## 4) build up different datasets as if we were moving chronologically in time
  ##    e.g. 2000. 2000 & 2005. 2000, 2005 & 2010. ...
  ##
  ## INPUTS:
  ## yr: 1 by #datapt vector containing the year for each datapt
  ##
  ## OUTPUTS: list with as many entries as unique years
  ## each item in the list contains all data rows that should be in that fold
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  ## get unique years
  yrs <- sort(unique(yr))

  ## make the list
  chronos <- list(NULL)
  for (y in yrs) {
    chronos[[which(yrs == y)]] <- which(yr <= y)
  }

  return(chronos)
}
