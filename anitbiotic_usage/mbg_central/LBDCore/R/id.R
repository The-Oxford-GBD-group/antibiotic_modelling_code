#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname id
#' @export
id <- function(q, ...) {
  ## get ids for each point

  d <- data.frame(id = NULL, x = NULL, y = NULL)
  if (class(q) == "quadtree") f <- id.quadtree else f <- id.quadtree.leaf
  f(q, xylim, ...)
}
