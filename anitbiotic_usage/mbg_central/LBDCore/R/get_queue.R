#' @title Get queue
#'
#' @description Get queue based on user supplied queue, run-time, and whether to use geos nodes or c2 nodes
#' If queue is already supplied by the user, then we just validate that string and return it
#'
#' @param use_geo_nodes Boolean. Use geo nodes?
#' @param use_c2_nodes Boolean. Use c2 nodes? Only relevant to cluster-prod.
#' @param run_time String. User supplied run-time
#' @param queue String. User supplied queue.
#'
#' @return A queue string.
#'
#' @importFrom stringr str_split
#' @export
get_queue <- function(use_geo_nodes, use_c2_nodes, queue = NULL, run_time = NULL) {

  ## Legacy cluster chunk:
  if (!is_new_cluster()) {
    if (use_geo_nodes) {
      return("geospatial.q")
    } else if (use_c2_nodes) {
      return("all.q@@c2-nodes")
    } else {
      return("all.q")
    }
  } else {

    ## All about fair cluster now

    ## If queue is supplied, then validate that string and return it:
    if (!is.null(queue)) {

      ## If use_geo_nodes is TRUE, then we always want to return geospatial.q
      if (use_geo_nodes) {
        message("use_geo_nodes was found to be TRUE. Returning geospatial.q regardless of the queue supplied.")
        return("geospatial.q")
      }

      return(validate_queue_string(queue))
    } else {
      ## If queue is null, then infer the queue based on the booleans and
      ## supplied runtime


      ## Split out the validated runtime into hours
      rt_hrs <- dhms_to_hours(
        get_run_time(use_geo_nodes = use_geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
      )

      ## Use the booleans to derive the proper queue:
      if (use_geo_nodes) {
        if (rt_hrs > 25 * 24) {
          stop("No queues exist with run-time greater than 25 days. Exiting.")
        }
        return("geospatial.q")
      } else {
        if (rt_hrs > 16 * 24) {
          stop("You are asking for a run-time of greater than 16 days but not asking for geospatial.q. Exiting.")
        } else if (rt_hrs <= 3 * 24) {
          return("all.q")
        } else {
          return("long.q")
        }
      }
    }
  }
}
