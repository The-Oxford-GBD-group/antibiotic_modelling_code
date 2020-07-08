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
#' @rdname vertices
#' @export
vertices <- function(x) {
  sum(sapply(x@polygons[[1]]@Polygons, function(y) nrow(y@coords)))
}
