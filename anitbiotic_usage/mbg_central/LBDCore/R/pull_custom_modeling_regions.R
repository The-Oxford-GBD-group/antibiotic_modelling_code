#' @title Pull custom modeling regions
#'
#' @description Define modeling regions that are not simple combinations of the
#'   default MBG regions (in other words, regions that are not combinations of
#'   four-letter MBG regions such as "wssa" or "seas+ocea" or ISO-3 codes such
#'   as 'ZAF' or 'CHN').
#'
#' @param custom_region character vector of custom modeling regions
#'
#' @return Returns a named list of custom modeling regions with associated
#'    "standard" (non-custom) modeling regions that can be directly interpreted
#'    by get_adm0_codes().
#'
#' @export
pull_custom_modeling_regions <- function(custom_regions) {
  custom_regions <- tolower(custom_regions)

  # Get region list
  data(ref_reg_list)

  # Warn if there are any custom regions not in the reference list
  missing_regions <- custom_regions[ !(custom_regions %in% names(ref_reg_list)) ]
  if (length(missing_regions) > 0) {
    message(paste0(
      "WARNING: The following custom regions are not defined: ",
      paste(missing_regions, collapse = ",")
    ))
  }
  # Return a named list of all custom regions
  custom_regions_list <- ref_reg_list[ names(ref_reg_list) %in% custom_regions ]
  return(custom_regions_list)
}
