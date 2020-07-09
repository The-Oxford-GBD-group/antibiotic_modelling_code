#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param draws PARAM_DESCRIPTION
#' @param target_type PARAM_DESCRIPTION
#' @param target PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_p_target
#' @export
get_p_target <- function(draws, target_type, target) {
  ## Functions for comparison to SDG target


  ## Calculates probability of being at/over a given target

  # draws = row of n preds over which to apply the function
  # target_type = ">",">=","<","<="
  # target = SDG, etc - e.g. "0.8" for 80% vaccine coverage

  output <- sum(do.call(get(target_type), list(draws, target)))
  output <- output / length(draws)
  return(output)
}
