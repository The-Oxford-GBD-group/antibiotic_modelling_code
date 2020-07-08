#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lv PARAM_DESCRIPTION
#' @param notification PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname check_if_nonzero_exit
#' @export
check_if_nonzero_exit <- function(lv, notification) {
  for (i in 1:nrow(lv)) {
    es <- lv[i, "exit_status"]
    if (!is.na(es) & (es != 0)) {
      if (lv[i, "tries"] <= max_tries - 1) {
        qs <- lv[i, "the_qsub"]
        returned <- system(as.character(qs), intern = T)
        new_job_id <- as.numeric(str_match(returned, "Your job ([0-9]*) ")[, 2])
        if (notification == "pushover") {
          pushover_notify(paste0(
            "Resubmitted ", lv[i, "jobname"], " with new job id ", new_job_id, ". ",
            "Failed job id: ", lv[i, "jobid"], " | exit status: ", lv[i, "exit_status"], "."
          ),
          title = paste0("Job failed: ", lv[i, "jobname"])
          )
        }
        lv[i, "exit_status"] <- NA
        lv[i, "jobid"] <- new_job_id
        lv[i, "tries"] <- lv[i, "tries"] + 1
      } else if (lv[i, tries] == max_tries) {
        if (lv[i, give_up] == F) {
          if (notification == "pushover") {
            pushover_notify(paste0(
              "Job ", lv[i, "jobname"], " was resubmitted ", max_tries, " times and will not be resubmitted again.",
              "Most recent job id was ", lv[i, "jobid"], "."
            ))
          }
          lv[i, give_up := TRUE]
        }
      }
    } # close if statement to catch non-zero exit statuses
  } # close for loop over lv rows
  return(lv)
} # close check_if_nonzero_exit()
