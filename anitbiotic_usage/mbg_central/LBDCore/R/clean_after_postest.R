#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param strata PARAM_DESCRIPTION
#' @param delete_region_rasters PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname clean_after_postest
#' @export
clean_after_postest <- function(indicator,
                                indicator_group,
                                run_date,
                                strata,
                                delete_region_rasters = F) {

  # Delete intermediate files that we no longer need
  main_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/")
  temp_dir <- paste0(main_dir, "temp_post_est/")

  # Deleting - be careful!
  unlink(temp_dir, recursive = T)

  # If desired, insert code here to delete other temporary objects (eg. region-specific rasters)
  grep_string <- paste0(indicator, "_(", paste(strata, collapse = "|"), ").*_raster.tif")
  region_rasters <- grep(grep_string, list.files(main_dir), value = T) %>%
    paste0(main_dir, .)
  if (delete_region_rasters == T) unlink(region_rasters)
}
