#' @title Adds environmental variables to a qsub string
#'
#' @description \code{qsub_sing_envs} assumes that a qsub string is being built to launch a
#' Singularity container. It always adds in the '-v sing_image=sing_image' as
#' expected by lbd_core/mbg_central/shell_sing.sh script that ultimately
#' launches the container. Optionally, users may want to pass the additional
#' environmental variables 'SET_OMP_THREADS' and/or 'SET_MKL_THREADS' to
#' shell_sing.sh. If one or both of those are passed into \code{qsub_sing_envs}
#' it will add those environmental variables and their values as additional
#' '-v' flags in the construction of the qsub command.
#'
#' @param qsub_stub A string containing the initial qsub string to which
#'   environmental variables will be concatenated to in the form of
#'   '-v ENV=VALUE'.
#'
#' @param envs This should be a named list of environmental variables.
#'   \code{qsub_sing_envs} will check that the names of the list members passed
#'   in match the environmental variables that the shell_sing.sh script knows
#'   about: 'SET_OMP_THREADS' and/or 'SET_MKL_THREADS'. Passing in other
#'   environmental names in the list will result in an error. If this is left
#'   as 'NULL' and a Singularity image is used, SET_OMP_THREADS and
#'   SET_MKL_THREADS will remain unset and the shell_sing.sh script will use
#'   the default setting of SET_OMP_THREADS=1 and SET_MKL_THREADS={max_threads}
#'   (see shell_sing.sh comments). For example SET_OMP_THREADS=1 and
#'   SET_MKL_THREADS=4 can be achieved by passing in
#'     \code{envs = list(SET_OMP_THREADS=1, SET_MKL_THREADS=4)}
#'
#' @param image The keyword (e.g. 'default') or path to the Singularity image.
#'   This should have been defined by \code{get_singularity} so likely
#'   \code{get_singularity} should have been run on the 'singularity' argument
#'   (in \code{make_qsub_share} or \code{parallelize} for example) before this
#'   function is run.
#'
#' @return Returns a string with at least '-v sing_image=image' and possibly
#'   other environmental variables values if they were passed into the
#'   'singularity_opts' argument of functions like \code{make_qsub_share}.
#'
#' @seealso The function \code{\link{get_singularity}} should likely be run
#'   before this function is run. This function is used by:
#'     \code{\link{parallelize}}
#'     \code{\link{make_qsub}}
#'     \code{\link{make_qsub_share}}
#'     \code{\link{make_qsub_postest}}
#'     \code{\link{submit_aggregation_script}}
#'
#' @export
qsub_sing_envs <- function(qsub_stub, envs, image) {
  # we always want to add this to our qsub to launch a Singularity container
  qsub_envs <- c(sing_image = image)
  # valid variables in the list passed into 'envs' argument
  valid_env_vars <- c("SET_OMP_THREADS", "SET_MKL_THREADS")
  if (!is.null(envs)) {
    # verify first that what is passed in is a list since this is all this
    # function knows how to deal with
    if (!is.list(envs)) {
      message("Expected a list for 'envs' argument")
      stop("Exiting!")
    }
    # if invalid variables were passed in, give a message and exit
    else if (!all(names(envs) %in% valid_env_vars)) {
      message("The following variables were unexpected for the 'envs' argument:")
      for (env in names(envs)[!names(envs) %in% valid_env_vars]) {
        message(paste0("  ", env))
      }
      stop("Exiting!")
    } else {
      qsub_envs <- c(qsub_envs, envs)
    }
  }
  # Concatenate all of the variables to the qsub string with the '-v'
  qsub_stub <- paste0(
    qsub_stub,
    paste0(
      " -v ", names(qsub_envs), "=", qsub_envs,
      collapse = ""
    )
  )
  return(qsub_stub)
}
