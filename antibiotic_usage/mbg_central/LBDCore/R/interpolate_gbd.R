#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gbd PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname interpolate_gbd
#' @export
interpolate_gbd <- function(gbd) {
  new_gbd <- list()
  for (this_year in c(2000:2015)) {
    if (this_year %in% 2000:2004) copied_data <- gbd[year == 2000, ]
    if (this_year %in% 2005:2009) copied_data <- gbd[year == 2005, ]
    if (this_year %in% 2010:2014) copied_data <- gbd[year == 2010, ]
    if (this_year %in% 2015:2015) copied_data <- gbd[year == 2015, ]
    copied_data <- copied_data[, year := this_year]
    new_gbd[[as.character(this_year)]] <- copied_data
  }
  new_gbd <- rbindlist(new_gbd)
  new_gbd <- new_gbd[order(name, year)]
  new_gbd <- new_gbd[!(year %in% c(2000, 2005, 2010, 2015)), mean := NA]
  for (country in unique(new_gbd[, name])) {
    new_gbd <- new_gbd[name == country, mean := na.approx(new_gbd[name == country, mean])]
  }
  return(new_gbd)
}
