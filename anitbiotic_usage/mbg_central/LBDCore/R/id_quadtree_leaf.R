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
#' @rdname id.quadtree.leaf
#' @export
id.quadtree.leaf <- function(q, ...) {
  ## print(q$id) ## for debugging
  if (length(q$value) == 0) {
    data.frame(
      id = q$id,
      x = NA,
      y = NA
    )
  } else if (length(q$value) == 2) {
    data.frame(
      id = q$id,
      x = q$value[1],
      y = q$value[2]
    )
  } else {
    data.frame(
      id = q$id,
      x = q$value[, 1],
      y = q$value[, 2]
    )
  }
}
