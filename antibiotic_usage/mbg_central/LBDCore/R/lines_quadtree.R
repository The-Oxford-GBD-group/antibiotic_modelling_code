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
#' @rdname lines.quadtree
#' @export
lines.quadtree <- function(q, xylim, ...) {
  i <- q$index
  j <- 3 - q$index

  if (q$threshold > xylim[1, i]) lines(q$lower, clip1(xylim, i, FALSE), ...)
  if (q$threshold < xylim[2, i]) lines(q$upper, clip1(xylim, i, TRUE), ...)
  xlim <- xylim[, j]
  xy <- cbind(c(q$threshold, q$threshold), xlim)
  lines(xy[, order(i:j)], ...)
}
