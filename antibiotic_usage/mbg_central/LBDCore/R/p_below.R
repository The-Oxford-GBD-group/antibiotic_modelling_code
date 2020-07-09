#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @param equal_to PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname p_below
#' @export
p_below <- function(x, value, equal_to = F) {

  # probability <  (or <= if equal_to = T) target value
  value <- as.numeric(value)
  if (equal_to == T) output <- sum(x <= value)
  if (equal_to == F) output <- sum(x < value)

  output <- output / length(x)
  return(output)
}
