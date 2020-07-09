#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#' @rdname NewEst
#' @export
NewEst <- function(vals, weightval) {
  vals <- vals * weightval
  vals <- apply(vals, 2, sum)
  vals <- vals / sum(weightval)

  return(mean(vals))
}
