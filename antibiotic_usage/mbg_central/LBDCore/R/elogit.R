#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname elogit
#' @export
elogit <- function(y, n) log((y + 0.5) / (n - y + 0.5))
