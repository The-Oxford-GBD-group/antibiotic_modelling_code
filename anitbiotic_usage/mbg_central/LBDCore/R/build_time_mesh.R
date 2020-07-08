#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param periods PARAM_DESCRIPTION, Default: 1:4
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname build_time_mesh
#' @export
build_time_mesh <- function(periods = 1:4) {
  ## Create temporal mesh (defaulting to the four period U5M approach for now, come back and make more flexible later)
  mesh_t <- inla.mesh.1d(
    loc = c(periods),
    degree = 1,
    boundary = rep("free", 2)
  )
  return(mesh_t)
}
