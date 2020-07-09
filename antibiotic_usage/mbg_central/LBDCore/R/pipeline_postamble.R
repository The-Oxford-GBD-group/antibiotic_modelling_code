#' @title Pipeline Postable
#'
#' @description Set up start job marker, load the DAG object, and assign
#' the correct constants in the global environment for the node
#'
#' @param headnode If TRUE, then all of the objects are saved out, and we assume
#' that this is the head node in the graph. Otherwise, we save a diff of
#' \code{ls()} and the \code{current_env}
#' Default: \code{FALSE}
#'
#' @param addl_objs_to_save Add additional objects to save if desired.
#' Default: \code{NULL}.
#'
#' @return NULL
#'
#' @export
pipeline_postamble <- function(headnode = FALSE, addl_objs_to_save = NULL) {

  ## Update job timer
  toc(log = TRUE)
  assign(
    paste0(
      nodename, "_time_elapsed"
    ),
    tic.log()[[1]],
    envir = .GlobalEnv
  )

  ## Temp variable for end job marker
  end_marker <- DAG_obj$tmpdir

  ## Save out environment, without the DAG
  rm(DAG_obj, envir = .GlobalEnv)


  ## Resolve object names
  if (headnode) {
    objs_to_save <- ls(envir = .GlobalEnv)
  } else {
    objs_to_save <- setdiff(ls(envir = .GlobalEnv), current_env)
  }

  ## If the user wants to add more objects to be saved out,
  ## then we concatenate that with `objs_to_save`, and call
  ## `unique`
  if (!is.null(addl_objs_to_save)) {
    objs_to_save <- unique(
      c(objs_to_save, addl_objs_to_save)
    )
  }

  mbg_save_nodeenv(
    node = nodename,
    ig = indicator_group,
    indic = indicator,
    rd = run_date,
    reg = reg,
    age = age,
    holdout = holdout,
    objs = objs_to_save
  )

  ## Create output file and remove err file ##
  mbg_job_marker(type = "end", tmpdir = end_marker)

  return(0)
}
