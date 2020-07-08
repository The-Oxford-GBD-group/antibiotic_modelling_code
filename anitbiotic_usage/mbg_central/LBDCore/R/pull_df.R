#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname pull_df
#' @export
pull_df <- function(region) {
  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, "_bin0_", region, "_0.RData"))
  return(df)
}
