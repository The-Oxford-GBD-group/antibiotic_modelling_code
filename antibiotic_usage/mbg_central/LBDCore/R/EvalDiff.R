#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gbdval PARAM_DESCRIPTION
#' @param vals PARAM_DESCRIPTION
#' @param weightval PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname EvalDiff
#' @export
EvalDiff <- function(gbdval, vals, weightval) {
  return(gbdval - NewEst(vals, weightval))
}
