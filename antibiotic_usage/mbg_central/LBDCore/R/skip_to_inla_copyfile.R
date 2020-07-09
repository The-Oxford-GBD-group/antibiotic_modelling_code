#' @title Copy Pre-prepped INLA image
#' @description Copy Pre-prepped INLA image from a specified rundate into current rundate
#'
#' @param indicator_group Indicator Group
#' @param indicator Indicator Name
#' @param run_date Current model run-date
#' @param skiptoinla_from_rundate A run-date which had INLA info saved out
#' @param pathaddin Additional thing to add to path, quite often: \code{paste0("_bin", age, "_", reg, "_", holdout)}
#'
#' @return NULL
#'
#' @export
#'
skip_to_inla_copyfile <- function(indicator_group, indicator, run_date, skiptoinla_from_rundate, pathaddin) {
  print("Copying INLA-prepped image from this file: ")
  print(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", skiptoinla_from_rundate, pathaddin, ".RData"))
  file.copy(
    from = paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", skiptoinla_from_rundate, pathaddin, ".RData"),
    to = paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, pathaddin, ".RData")
  )
  return(0)
}