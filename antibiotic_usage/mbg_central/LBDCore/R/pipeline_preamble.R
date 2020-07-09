#' @title Pipeline Preamble
#'
#' @description Set up start job marker, load the DAG object, and assign
#' the correct constants in the global environment for the node
#'
#' @param headnode If TRUE, then the previous node's environment is not loaded
#' and we assume that this is the head node in the graph.
#' Default: \code{FALSE}
#'
#' @return NULL
#' @export
#'
pipeline_preamble <- function(headnode = FALSE) {

  ## Start timer
  tic()

  ## Load DAG into an object called DAG_obj
  assign("DAG_obj", setup_parallel_model_inputs(
    load_prerun_image = TRUE,
    use_argparse = TRUE
  ), envir = .GlobalEnv)

  ## Set up job tmpdir and create start file in temp directory defined in DAG
  mbg_job_marker(type = "start", tmpdir = DAG_obj$tmpdir)


  ## If not head node, then do all this below:
  if (!headnode) {

    ## Get parent nodenames name and the environment saved from parent jobs
    assign("parent_nodename",
      DAG_obj$get_all_parent_nodes(
        name = nodename
      ),
      envir = .GlobalEnv
    )

    mbg_get_nodeenv(
      node = parent_nodename,
      ig = indicator_group,
      indic = indicator,
      rd = run_date,
      reg = reg,
      age = age,
      holdout = holdout
    )

    ## Take note of the current state of the environment (to use as diff-loading)
    assign("current_env", ls(envir = .GlobalEnv), envir = .GlobalEnv)

  }

  return(0)
}
