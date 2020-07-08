#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param region PARAM_DESCRIPTION
#' @param agebin PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_cell_preds_stack
#' @export
load_cell_preds_stack <- function(indicator_group,
                                  indicator,
                                  rd = run_date,
                                  region,
                                  agebin) {
  load(paste0(
    "/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", rd, "/",
    indicator, "_cell_draws_eb_bin", agebin, "_", region, "_0_stacked_results.RData"
  )) # the 0 are no holdouts

  return(cell_pred)
}
