#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param the_poly PARAM_DESCRIPTION
#' @param compare_to PARAM_DESCRIPTION
#' @param the_proj PARAM_DESCRIPTION, Default: projection(master_shape)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[rgeos]{gIntersection}}
#'  \code{\link[raster]{area}}
#' @rdname check_if_in_spoly
#' @export
#' @importFrom rgeos gIntersection
#' @importFrom raster area
check_if_in_spoly <- function(the_poly, compare_to, the_proj = projection(master_shape)) {
  the_poly <- SpatialPolygons(list(Polygons(list(the_poly), ID = 1)))
  projection(the_poly) <- the_proj
  projection(compare_to) <- the_proj

  if (suppressWarnings(gIsValid(the_poly)) == F) return(TRUE) # Ignore invalid polygons

  poly_intersect <- rgeos::gIntersection(the_poly, compare_to)

  if (is.null(poly_intersect)) {
    return(FALSE)
  } else {
    return(ifelse((raster::area(poly_intersect) == raster::area(the_poly)), TRUE, FALSE))
  }
}
