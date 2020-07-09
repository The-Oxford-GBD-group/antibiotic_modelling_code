#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param xylim PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cell
#' @export
cell <- function(q, xylim, ...) {
  ## get cell boundaries to make shapefiles

  if (class(q) == "quadtree") f <- cell.quadtree else f <- cell.quadtree.leaf
  f(q, xylim, ...)
}
