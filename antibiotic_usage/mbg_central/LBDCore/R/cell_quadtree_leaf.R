#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param xylim PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cell.quadtree.leaf
#' @export
cell.quadtree.leaf <- function(q, xylim) {
  data.frame(
    id = q$id,
    x = c(xylim[1, 1], xylim[2, 1], xylim[2, 1], xylim[1, 1], xylim[1, 1]),
    y = c(xylim[1, 2], xylim[1, 2], xylim[2, 2], xylim[2, 2], xylim[1, 2])
  )
}
