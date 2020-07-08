#' @title Get model output directory
#'
#' @description return path to model output directory.
#'
#' @param indicator_group the indicator group e.g., "hiv" or "u5m".
#'
#' @param indicator the indicator e.g., "wasting_mod_b" or "male_circumcision".
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @return the path to the models output directory.
#'
#' @export
get_model_output_dir <- function(indicator_group, indicator, run_date) {
  path_join(get_indicator_dir(indicator_group, indicator), "output", run_date)
}
