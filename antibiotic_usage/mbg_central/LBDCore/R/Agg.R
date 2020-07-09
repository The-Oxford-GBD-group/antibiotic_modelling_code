#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p_i PARAM_DESCRIPTION
#' @param N_i PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname Agg
#' @export
Agg <- function(p_i, N_i) {
  return(sum(p_i * N_i) / sum(N_i))
}
