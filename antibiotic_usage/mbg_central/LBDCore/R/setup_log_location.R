#' @title Set up log location
#'
#' @description Determines error/output file path and optionally ensures dirs exist.
#'
#' @param log_location the location of the log files OR 'sgeoutput' OR
#'  'sharedir'. 'sgeoutput' and 'sharedir' will appropriately substitute the
#'  user's default sgeoutput directory or the models default output directory
#'  (respectively).
#'
#' @param user the name of the user.
#'
#' @param indicator the indicator e.g., "wasting_mod_b" or "male_circumcision".
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @param age the age for the model which uses this data.
#'
#' @param make_dirs whether to make the directories. [default = TRUE]
#'
#' @export
setup_log_location <- function(log_location, user, indicator, indicator_group, run_date, make_dirs = TRUE) {
  if (log_location == "sgeoutput") {
    log_root <- get_sge_output_dir(user)
  } else if (log_location == "sharedir") {
    log_root <- get_model_output_dir(indicator_group, indicator, run_date)
  } else {
    log_root <- log_location
  }
  output_err <- c(path_join(log_root, "output"), path_join(log_root, "errors"))
  if (make_dirs) sapply(output_err, dir.create, showWarnings = FALSE)
  return(output_err)
}
