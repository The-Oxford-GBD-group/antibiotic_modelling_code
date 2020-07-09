#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yr PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION, Default: NULL
#' @param ss PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname proptime_folds
#' @export
proptime_folds <- function(yr, n_folds = NULL, ss = NULL, ...) {

  ##############
  ## IN  TIME ##
  ##############

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## 1) random in time - already done with completely random
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## 2) proportional to data amount in the period
  ## already done. this is equivalent to stratifying by time the
  ## stratified holdouts can then be recombined across time to yield
  ## proportional to data in the time period
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


  ## so all we have to do is return a 'fold vector' with integers for unique years

  yrf <- as.factor(yr)
  fold_vec <- as.numeric(yrf)

  fold_list <- list(NULL)
  for (i in sort(unique(fold_vec))) {
    fold_list[[i]] <- which(fold_vec == i)
  }
  return(fold_list)
}
