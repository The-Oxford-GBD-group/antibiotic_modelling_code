#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cirange
#' @export
cirange <- function(x) {
  z <- quantile(x, probs = c(.025, .975), na.rm = T)
  return(z[2] - z[1])
}
