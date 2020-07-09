#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param alpha PARAM_DESCRIPTION, Default: 0.1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @rdname points_quadtree_leaf
#'
#' @export
#' 
points_quadtree <- function(q, alpha = 0.1, ...) {
  graphics::points(q$lower, alpha, ...)
  graphics::points(q$upper, alpha, ...)
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
#' 
points.quadtree <- function(...) {
  points_quadtree(...)
}
