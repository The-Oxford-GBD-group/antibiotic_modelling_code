#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION, Default: 1
#' @param k PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname quad1
#' @export
quad1 <- function(xy, i, id = 1, k) {
  if (nrow(xy) < k * d) {
    rv <- list(id = id, value = xy)
    class(rv) <- "quadtree.leaf"
  }
  else {
    q0 <- (1 + runif(1, min = -1 / 2, max = 1 / 2) / dim(xy)[1]) / 2 # Random quantile near the median
    x0 <- quantile(xy[, i], q0)
    j <- i %% d + 1 # (Works for octrees, too...)
    rv <- list(
      index = i, threshold = x0,
      lower = quad(xy[xy[, i] <= x0, ], j, id * 2),
      upper = quad(xy[xy[, i] > x0, ], j, id * 2 + 1)
    )
    class(rv) <- "quadtree"
  }
  return(rv)
}
