#' @title Get Resources
#'
#' @description Get list of resources to request for new cluster.
#'
#' @param use_geo_nodes logical in dicating if code should run on LBD nodes.
#'
#' @param cores numeric indicating the number of cores to request.
#'
#' @param ram_gb numeric indicating the number of GB of RAM to request.
#'
#' @param runtime (default NULL) runtime in 'DD:HH:MM:SS' format.
#'
#' @return named vector with resource name and string values.
#'
#' @rdname get_resources
#' @export
#'
get_resources <- function(..., use_geo_nodes, cores, ram_gb, runtime = NULL) {
  result <- c()
  if (is_new_cluster()) {
    result <- c(result, c(
      archive = "TRUE",
      m_mem_free = paste0(ram_gb, "G"),
      fthread = cores
    ))
    # set default value for runtime if not provided. These are the maximums per
    # https://docs.cluster.ihme.washington.edu/allocation-and-limits/queues/
    if (is.null(runtime)) {
      if (use_geo_nodes) {
        runtime <- "25:00:00:00"
      } else {
        runtime <- "3:00:00:00"
      }
    } else {
      runtime <- get_run_time(use_geo_nodes, use_c2_nodes, run_time = runtime)
    }
    result <- c(result, c(h_rt = runtime))
  } else {
    result <- c(result, c(mem_free = paste0(ram_gb, "G")))
    if (use_geo_nodes) result <- c(result, c(geos_node = "TRUE"))
  }
  return(result)
}
