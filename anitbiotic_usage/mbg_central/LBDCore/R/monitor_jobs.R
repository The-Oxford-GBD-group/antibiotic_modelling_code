#' @title Monitor and resubmit jobs
#' @description Function to monitor & resubmit jobs if they fail
#' This is intended as a replacement for \code{waitformodelstofinish()}
#' and its ilk.  This function takes output from \code{parallelize()}
#' and periodically submits qstat and qacct requests to see how
#' the jobs are running on the cluster.
#'
#' @param parallelize_output output from the `parallelize()` function, where
#'                           `parallelize_output[[1]] is a data.table of loopvars
#'                           and `parallelize_output[[2]] is the filename that
#'                           contains the `save_objs` from parallelize
#' @param sleeptime how long to sleep for between checking the status of the jobs
#'                  running on the cluster? (numeric, in seconds)
#' @param title title for the looping output
#' @param keep_temp_file keep the temp file after this function exits?
#'                       logical; if `keep_temp_file = F` then temp file is deleted
#' @param return_lv should this function return the loopvars?
#' @param max_tries maximum number of times to resubmit a job before giving up
#' @param notification how would you like to be notified when jobs fail?
#'                     The only current option is "pushover" (via `pushover_notify()`)
#'                     but could be expanded to include email, etc. if desired
#' @return loopvars in data table (if `return_lv = T`)
#' @examples
#' \dontrun{
#' # see example for `parallelize()` for workflow
#' # In master script:
#' monitor_jobs(output_from_parallelize,
#'   max_tries = 3,
#'   notification = "pushover"
#' )
#' }
#' @export
monitor_jobs <- function(parallelize_output,
                         sleeptime = 100,
                         title = "Job Monitor",
                         keep_temp_file = F,
                         return_lv = F,
                         max_tries = 1,
                         notification = "none") {
  lv <- parallelize_output[[1]]
  fname <- parallelize_output[[2]]

  str_match <- stringr::str_match

  # Wait a minute to let all jobs be submitted
  Sys.sleep(60)

  # Add a column to lv to hold exit statuses
  if (!("exit_status" %in% names(lv))) lv[, exit_status := numeric()]
  if (!("tries" %in% names(lv))) lv[, tries := 1]
  if (!("give_up" %in% names(lv))) lv[, give_up := F]

  # Updating of loopvars table
  lv <- update_lv_table(lv)
  n_finished <- nrow(lv[state == "x" & exit_status == 0, ])

  while (n_finished < nrow(lv)) {
    lv <- update_lv_table(lv)
    n_finished <- nrow(lv[state == "x" & exit_status == 0, ])

    message("\n====================================================================================")
    message(sprintf("==============================      %s      ===============================", title))
    message(paste0("\nAt ", Sys.time(), " .... ", n_finished, " of ", nrow(lv), " jobs have finished."))
    message("\nJob status:")
    for (i in 1:nrow(lv)) {
      message(paste0(
        "  Job: ", lv[i, "jobname"],
        " | ID: ", lv[i, "jobid"],
        " | Tries: ", lv[i, "tries"],
        " | State: ", lv[i, "state"],
        " | Exit status: ", lv[i, "exit_status"]
      ))
    }
    message("\n====================================================================================")
    message("====================================================================================")
    message("\n")

    lv <- check_if_nonzero_exit(lv, notification)
    Sys.sleep(sleeptime)
  } # close while loop
  # Exiting function...
  if (return_lv == T) return(lv)
  if (keep_temp_file == F) unlink(paste0("/share/geospatial/tmp/", fname, ".RData"))
}
