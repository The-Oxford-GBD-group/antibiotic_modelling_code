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
#' @rdname addQuotes
#' @export
addQuotes <- function(x) {
  # If x is a character string, add (escaped) quotation marks
  if (is.character(x)) {
    x <- sprintf('\"%s\"', x)
  }
  return(x)
}
