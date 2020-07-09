#' @title Get project
#'
#' @description Return default project if not set.
#'
#' @param proj str project name (may be NULL)
#'
#' @param use_geo_nodes logical indicating whether to use geospatial nodes.
#'
#' @return proj (if not null) or an appropriate default project.
#'
#' @export
get_project <- function(proj, use_geo_nodes) {
  if (!is.null(proj)) return(proj)
  if (use_geo_nodes) "proj_geo_nodes" else "proj_geospatial"
}
