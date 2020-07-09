#' @title Validate node option
#'
#' @description Validates node selection emitting warning message if inconsistent.
#'
#' @param use_geo_nodes logical indicating whether to use geospatial nodes.
#'
#' @param use_c2_nodes logical indicating whether to use "c2-" prefixed nodes.
#'
#' @export
validate_node_option <- function(use_geo_nodes, use_c2_nodes, project) {
  if (use_geo_nodes & use_c2_nodes) {
    message("WARNING: Both 'geo_nodes' and 'use_c2_nodes' arguments were set to TRUE")
    message(paste0("         Submitting job to LBD nodes under project: '", project, "'"))
  }
}
