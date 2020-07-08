#' @title get_indicator_dir
#'
#' @description Compute the directory for outputs for a given indicator
#'
#' @param indicator_group the indicator group e.g., "hiv" or "u5m".
#'
#' @param indicator the indicator e.g., "wasting_mod_b" or "male_circumcision".
#'
#' @return filesystem path.
#'
#' @export
get_indicator_dir <- function(indicator_group, indicator) {
  path_join(MBG_ROOT, indicator_group, indicator)
}
