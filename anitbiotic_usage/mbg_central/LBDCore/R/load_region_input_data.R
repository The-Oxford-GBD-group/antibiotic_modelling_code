#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_region_input_data
#' @export
load_region_input_data <- function(indicator_group, indicator, run_date, reg) {
  model_path <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  df <- list.files(model_path, pattern = paste0("input_data[a-zA-Z0-9_]*", reg), full.names = T) %>% fread()
  return(df)
}
