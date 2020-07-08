#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param K PARAM_DESCRIPTION
#' @param vals PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname NewPixelVal
#' @export
NewPixelVal <- function(K, vals) {
  return(ilogit(logit(vals) + K))
}
