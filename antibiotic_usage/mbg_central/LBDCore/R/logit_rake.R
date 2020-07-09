
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param country_year PARAM_DESCRIPTION
#' @param rake_dt PARAM_DESCRIPTION
#' @param gbd PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname logit_rake
#' @export
logit_rake <- function(country_year, rake_dt, gbd) {
  ## Actual raking function
  rake_to_country <- strsplit(country_year, "_")[[1]][[1]]
  rake_to_year <- strsplit(country_year, "_")[[1]][[2]]
  p_N <- gbd[name == rake_to_country & year == rake_to_year, mean]
  raking_factors <- FindK(
    p_i = rake_dt[, p_i],
    p_N = p_N,
    N_i = rake_dt[, sim_N_i],
    a = 2
  )
  rf <- as.data.table(rake_to_country)
  setnames(rf, "rake_to_country", "name")
  rf <- rf[, year := rake_to_year]
  rf <- rf[, raking_factor := raking_factors]
  return(rf)
}
