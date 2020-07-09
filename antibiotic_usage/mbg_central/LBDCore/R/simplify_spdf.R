#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param the_spdf PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname simplify_spdf
#' @export
simplify_spdf <- function(the_spdf, ...) {
  # Wrapper for gSimplify() to allow it to work with SPDFs
  simple_spdf <- gSimplify(the_spdf, topologyPreserve = T, ...)
  return(SpatialPolygonsDataFrame(simple_spdf, the_spdf@data))
}
