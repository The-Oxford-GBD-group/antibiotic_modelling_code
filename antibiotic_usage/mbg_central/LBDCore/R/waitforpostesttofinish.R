#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sleeptime PARAM_DESCRIPTION, Default: 100
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_group PARAM_DESCRIPTION, Default: indicator_group
#' @param run_date PARAM_DESCRIPTION, Default: run_date
#' @param strata PARAM_DESCRIPTION
#' @param showfiles PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname waitforpostesttofinish
#' @export
waitforpostesttofinish <- function(sleeptime = 100,
                                   indicator = indicator,
                                   indicator_group = indicator_group,
                                   run_date = run_date,
                                   strata,
                                   showfiles = TRUE) {
  path <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/temp_post_est/")

  lv <- data.table(strata)
  names(lv) <- "stratum"

  lv[, filename := paste0(path, stratum, "_post_est_list.RData")]

  n_finished <- 0
  n_needed <- length(strata)

  while (n_finished != n_needed) {
    lv[, exists := file.exists(filename)]
    n_finished <- nrow(lv[exists == T])

    message("\n====================================================================================")
    message("\nRunning post-estimation in parallel")
    message(paste0("Run Date: ", run_date))
    message(paste0("\nAt ", Sys.time(), " .... ", n_finished, " out of ", n_needed, " strata have written output."))
    if (showfiles) {
      message(paste0("\nCurrently missing strata:"))
      for (i in 1:nrow(lv[exists == F])) {
        message(subset(lv, exists == F)[i, stratum])
      }
    }
    message("\n====================================================================================")
    message("====================================================================================")
    Sys.sleep(sleeptime)
  }
}
