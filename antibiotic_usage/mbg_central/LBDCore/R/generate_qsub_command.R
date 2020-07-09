#' @title Generates a qsub command that should work on any IHME cluster.
#'
#' @param ... The command + arguments to be run as a hibh e.g., the
#'  singularity shell, the parallel script and its 8 arguments.
#'
#' @param stderr_log string indicating directory or file to log stderr to.
#'
#' @param stdout_log string indicating directory or file to log stdout to.
#'
#' @param project the project to run the job under.
#'
#' @param resources named vector of resources.
#'  @seealso \code{\link{get_resources}}
#'
#' @param job_name string name for the job.
#'
#' @param singularity_opts string options to pass to singularity.
#'  @seealso \code{\link{qsub_sing_envs}}
#'
#' @param cores numeric number of cores to use
#'
#' @param priority Job priority that can be deprioritized if needed, and can only be used for values in [-1023,0]. Default = 0.
#' This value will get bounded to 0 or -1023 if the user supplies a value outside those bounds.
#' 
#' @param hold_jid Job names or job ID to hold on (uses \code{-hold_jid the_held_job_names}). Must be a vector if multiple jobs specified
#'
#' @export
generate_qsub_command <- function(..., stderr_log, stdout_log, project, resources, job_name, singularity_str, queue, cores = NULL, priority = 0, hold_jid = NULL) {

  # Check on priority value
  if (priority < -1023) {
    warning("Priority value supplied is < -1023. Setting to -1023 (lowest possible value).")
    priority <- -1023
  } else if (priority > 0) {
    warning("Priority value supplied is > 0 Setting to 0 (highest possible value).")
    priority <- 0
  }

  new_cluster <- is_new_cluster()
  if (!new_cluster & is.null(cores)) stop("cores not specified, but is mandatory on old cluster.")
  resource_str <- get_resource_str(resources)
  paste(
    "qsub",
    # qsub arguments
    "-e", stderr_log, "-o", stdout_log, "-P", project, "-N", job_name, "-q", queue,
    "-cwd", resource_str, singularity_str,
    "-p", priority,
    # shim for old cluster slot
    if (!new_cluster) {
      paste("-pe multi_slot", cores)
    },
    if(!is.null(hold_jid)) {
      paste0("-hold_jid ",
             paste(hold_jid, collapse = ","))
    },
    # command + arguments
    ...
  )
}
