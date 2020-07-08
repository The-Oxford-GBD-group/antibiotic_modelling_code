#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION, Default: data
#' @param strat_cols PARAM_DESCRIPTION, Default: strat_cols
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_strat_combos
#' @export
get_strat_combos <- function(data = data,
                             strat_cols = strat_cols,
                             ...) {

  ## this function creates all unique combos that we need to stratify over when making folds
  ## e.g. if gender and age_bin vectors are specified this function
  ## will return a list with all unique combos of age_bin and gender

  ## get all unique items from  each column
  unique_list <- list(NULL)
  for (i in 1:length(strat_cols)) {
    unique_list[[i]] <- sort(unique(data[, strat_cols[i]]))
  }

  ## make a dataframe of all combos and return it
  all_combos <- expand.grid(unique_list)
  colnames(all_combos) <- strat_cols
  return(all_combos)
}
