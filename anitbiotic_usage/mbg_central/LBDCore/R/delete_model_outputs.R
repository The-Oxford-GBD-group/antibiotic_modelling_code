#' @title Deletes model outputs from share directories
#' @description Quickly delete the model outputs of a number of model runs will create
#'  a unique combination for each indicator, indicator_group, run_date combination
#'
#'
#' @param indicator_group The indicator group level example "u5m"
#' @param indicator The indicator level example "died"
#' @param run_date The name of your model but often its just a run date
#' @param dryrun Only print the directories that would be deleted dont actually
#' @param ... Additional arguments passed to unlink function
#' @return None
#' @export
delete_model_outputs <- function(
                                 indicator_group, indicator, run_date, dryrun = FALSE, ...) {
  fileDF <- expand.grid(ind = indicator, ig = indicator_group, rd = run_date) %>%
    mutate(moddir = file.path("/share/geospatial/mbg", ig, ind, "output", rd))

  if (dryrun) {
    print("Will Delete the following directories")
    print(fileDF$moddir)
  }

  else {
    for (i in 1:nrow(fileDF)) {
      unlink(fileDF$moddir[i], recursive = T, ...)
      print("Deleted directory")
      print(fileDF$moddir[i])
    }
  }
}
