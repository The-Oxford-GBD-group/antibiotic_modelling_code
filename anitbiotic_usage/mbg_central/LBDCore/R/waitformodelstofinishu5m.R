#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sleeptime PARAM_DESCRIPTION, Default: 100
#' @param path PARAM_DESCRIPTION, Default: paste0("/share/geospatial/mbg/", indicator_group, "/", indicator,
#'    "/output/", run_date, "/")
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param lv PARAM_DESCRIPTION, Default: loopvars
#' @param ageasindic PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname waitformodelstofinishu5m
#' @export
waitformodelstofinishu5m <- function(sleeptime = 100,
                                     path = paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/"),
                                     rd = run_date,
                                     lv = loopvars,
                                     ageasindic = TRUE) {
  pattern <- "fin_"
  paths <- list()
  if (ageasindic) {
    for (a in unique(lv[, 2])) paths[[a]] <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "_age", a, "/output/", run_date, "/")
  } else {
    paths[[1]] <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/")
  }

  n_outputwritten <- 0
  while (n_outputwritten != nrow(lv)) {
    n_outputwritten <- 0
    for (i in 1:length(paths)) n_outputwritten <- n_outputwritten + length(list.files(path = paths[[i]], pattern = pattern))

    message("\n====================================================================================")
    message(sprintf("=====================      Run Date: %s      ======================", rd))
    message(paste0("\nAt ", Sys.time(), " .... ", n_outputwritten, " of ", nrow(lv), " Models have written output."))
    message("\nFuthermore, this many are still running on cluster:")
    system("qstat | grep job_ | wc | awk '{print $1}'")
    message("\nThe following are still running on cluster")
    system("qstat -xml | tr '\n' ' ' | sed 's#<job_list[^>]*>#\n#g'  | sed 's#<[^>]*>##g' | grep 'job_' | column -t | awk {'print $3'}")
    message("\n====================================================================================")
    message("====================================================================================")
    message("\n")

    if (n_outputwritten == nrow(lv)) next
    Sys.sleep(sleeptime)
  }
}
