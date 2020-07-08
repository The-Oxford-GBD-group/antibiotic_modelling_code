#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param region PARAM_DESCRIPTION
#' @param agebin PARAM_DESCRIPTION
#' @param u5m PARAM_DESCRIPTION, Default: FALSE
#' @param other PARAM_DESCRIPTION, Default: ''
#' @param ageasindic PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_cell_preds
#' @export
load_cell_preds <- function(indicator_group,
                            indicator,
                            rd = run_date,
                            region,
                            agebin,
                            u5m = FALSE,
                            other = "",
                            ageasindic = TRUE) {
  if (u5m) {
    if (ageasindic == FALSE) {
      load(paste0(
        "/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", rd, "/",
        indicator, "_cell_draws_eb_bin", agebin, "_", region, "_0", other, "NA.RData"
      )) # the 0 are no holdout
    } else {
      load(paste0(
        "/share/geospatial/mbg/", indicator_group, "/", indicator, "_age", agebin, "/output/", rd, "/",
        indicator, "_age", agebin, "_cell_draws_eb_bin", agebin, "_", region, "_0", other, ".RData"
      ))
    }
    cell_pred <- cptmp
  } else {
    load(paste0(
      "/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", rd, "/",
      indicator, "_cell_draws_eb_bin", agebin, "_", region, "_0.RData"
    )) # the 0 are no holdouts
  }
  return(cell_pred)
}
