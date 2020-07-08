#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param alpha PARAM_DESCRIPTION, Default: 0.1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @rdname points_quadtree_leaf
#'
#' @export
#' @importFrom graphics points
points_quadtree_leaf <- function(q, alpha = 0.1, ...) {
  graphics::points(q$value, col = alpha(q$id, alpha = alpha), ...)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param alpha PARAM_DESCRIPTION, Default: 0.1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @rdname points_quadtree_leaf
#'
#' @export
points.quadtree.leaf <- function(...) {
  points_quadtree_leaf(...)
}