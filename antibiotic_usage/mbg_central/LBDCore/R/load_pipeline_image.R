#' @title Load Pipeline Pre-saved Image
#' @description Load Pipeline Pre-saved image into the global environment
#'
#' @param indicator_group Indicator Group
#' @param indicator Indicator Name
#' @param run_date A pre-saved run-date
#' @param pathaddin Additional thing to add to path, quite often: \code{paste0("_bin", age, "_", reg, "_", holdout)}
#'
#' @return The pre-saved environment from launch script
#'
#' @export
#'
load_pipeline_image <- function(indicator_group, indicator, run_date, pathaddin, file_format = ".RData") {
  print("Loading pre-saved image into global environment from this file: ")
  print(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/DAG_", run_date, pathaddin, file_format))
  if (file_format == ".rds") {
    readRDS(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/DAG_", run_date, pathaddin, ".rds"))
  } else if (file_format == ".RData") {
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/DAG_", run_date, pathaddin, ".RData"), envir = .GlobalEnv)
  } else {
    stop("However, this file format not supported!!!!")
  }
}
