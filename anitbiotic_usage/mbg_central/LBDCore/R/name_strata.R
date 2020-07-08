#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param all_strat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname name_strata
#' @export
name_strata <- function(all_strat) {

  ## @title make strata names for final list of holdout datasets

  ## takes in a matrix containing combinations of all strata
  ##      (e.g. the output from get_strat_combos() )
  ## returns a vector containing the names of the strata
  ## NAMING CONVENTION: stratcol1__strata1___stratcol2__strata2___...


  as <- all_strat
  strat_names <- paste(colnames(as)[1], as[, 1], sep = "__")
  if (ncol(as) > 1) {
    for (i in 2:ncol(as)) {
      strat_names <- paste(strat_names,
        paste(colnames(as)[i], as[, i], sep = "__"),
        sep = "___"
      )
    }
  }

  return(strat_names)
}
