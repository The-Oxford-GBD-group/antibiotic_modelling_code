#' @title Get run-time
#'
#' @description Get run-time based on user supplied queue, run-time, and whether to use geos nodes or c2 nodes
#' If run-time is already supplied by the user, then we just validate that string and return it
#'
#' @param use_geo_nodes Boolean. Use geo nodes?
#' @param use_c2_nodes Boolean. Use c2 nodes? Only relevant to cluster-prod.
#' @param run_time String. User supplied run-time
#' @param queue String. User supplied queue.
#'
#' @return A run-time string in format \code{DD:HH:MM:SS} if using fair cluster. Returns \code{NULL} otherwise.
#' If run-time is empty, then it return the max runtime
#'
#' @export
get_run_time <- function(use_geo_nodes, use_c2_nodes, queue = NULL, run_time = NULL) {

  ## Get legacy cluster queue:
  if (!is_new_cluster()) {
    return(NULL)
  } else {
    ## If RT is null:
    if (is.null(run_time)) {
      if (is.null(queue)) {
        if (!use_geo_nodes | is.null(use_geo_nodes)) {
          message("No queue supplied. Reverting to long.q")
        }
        return(get_max_runtime_by_queue("long.q"))
      } else {
        if (use_geo_nodes) {
          if (!(queue %in% "geospatial" | queue %in% "geospatial.q")) {
            warning("You did not specify geospatial.q but supplied TRUE for use_geo_nodes. Reverting to the geospatial.q")
          }
          run_time <- get_max_runtime_by_queue("geospatial.q")
        } else {
          if (queue %in% "all.q") {
            run_time <- get_max_runtime_by_queue("all.q")
          } else {
            run_time <- get_max_runtime_by_queue("long.q")
          }
        }
      }
    } else {
      ## Nothing to do except just validate run_time and return below!
    }
    return(complete_runtime_string(run_time))
  }
}
