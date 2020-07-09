#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION, Default: 5
#' @param strat_cols PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname rand_folds
#' @export
rand_folds <- function(data,
                       n_folds = 5,
                       strat_cols = NULL,
                       ...) {

  ####################
  ## TOTALLY RANDOM ##
  ####################

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## makes folds completely at random across all data
  ##
  ## INPUTS:
  ## data: cleaned data.table/frame to be split into folds
  ## n_folds: number of folds
  ## strat_cols: vector of column string names to
  ##    stratify over when making folds. if NULL, holdout
  ##    sets are made across the entire data structure
  ##
  ## OUTPUTS: 2 item list
  ## 1st item: 1 by nrow(data) vector containing integers identifying folds
  ## 2nd item: matrix of stratification combinations used to make holdouts
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  ## make fold vectors stratifying by specified columns
  if (!is.null(strat_cols)) {

    ## get different stratum
    all_strat <- get_strat_combos(data = data, strat_cols = strat_cols)

    ## make a vector to identify folds and assign completely at random by strata
    fold_vec <- rep(NA, nrow(data))

    for (strat in 1:nrow(all_strat)) {
      strata <- as.data.frame(all_strat[strat, ])
      colnames(strata) <- colnames(all_strat)

      ## get rows in current strata
      strat_rows <- get_strat_rows(
        data = data,
        strata = strata
      )

      ## assign fold numbers uniformly (with rounding)
      fold_s <- cut(seq(1, sum(strat_rows)),
        breaks = n_folds, labels = 1:n_folds
      )
      fold_s <- as.numeric(as.character(fold_s))

      ## randomize the numbers within the strata
      fold_vec[which(strat_rows == 1)] <- sample(fold_s)
    }

    ## check to make sure we got all rows
    if (sum(is.na(fold_vec) > 0)) {
      message("Warning! Check why some data rows didn't get assigned to folds")
    }
  } else { ## make folds across all data
    fold_vec <- sample(cut(seq(1, nrow(data)),
      breaks = n_folds, labels = 1:n_folds
    ))
  }


  ## and return a list containing
  ## 1) vector of folds
  ## 2) matrix containing all stratum
  if (is.null(strat_cols)) {
    return(list(
      folds = fold_vec,
      stratum = NA
    ))
  } else {
    return(list(
      folds = fold_vec,
      stratum = all_strat
    ))
  }
}
