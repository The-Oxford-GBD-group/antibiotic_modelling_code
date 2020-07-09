#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param location_set_version_id PARAM_DESCRIPTION
#' @param year_start PARAM_DESCRIPTION
#' @param year_end PARAM_DESCRIPTION
#' @param by_sex PARAM_DESCRIPTION, Default: 1
#' @param by_age PARAM_DESCRIPTION, Default: 1
#' @param custom_sex_id PARAM_DESCRIPTION, Default: NULL
#' @param custom_age_group_id PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_demographics
#' @export
get_demographics <- function(location_set_version_id,
                             year_start, year_end,
                             by_sex = 1, by_age = 1,
                             custom_sex_id = NULL, custom_age_group_id = NULL) {
  ## Locations
  locs <- get_location_hierarchy(location_set_version_id)[level >= 3 & level < 6, ]$location_id
  ## Years
  years <- seq(year_start, year_end, 1)
  ## Sexes
  if (is.blank(custom_sex_id)) {
    if (by_sex == 1) {
      sexes <- c(1, 2)
    } else if (by_sex == 0) {
      sexes <- 3
    }
  } else {
    sexes <- custom_sex_id
  }
  ## Ages
  if (is.blank(custom_age_group_id)) {
    if (by_age == 1) {
      dbname <- "shared"
      host <- "modeling-cod-db.ihme.washington.edu"
      query <- "SELECT age_group_id FROM shared.age_group_set_list WHERE age_group_set_id=1"
      ages <- run_query(dbname, host, query)$age_group_id
    } else if (by_age == 0) {
      ages <- 22
    }
  } else {
    ages <- custom_age_group_id
  }

  ## Expand
  df <- data.table(expand.grid(location_id = locs, year_id = years, sex_id = sexes, age_group_id = ages))
  ## Force integer
  df <- df[, lapply(.SD, as.character)]
  df <- df[, lapply(.SD, as.integer)]

  return(df)
}
