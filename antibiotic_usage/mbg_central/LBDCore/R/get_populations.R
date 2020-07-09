#####################################################################################################################################################
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
#' @rdname get_populations
#' @export
get_populations <- function(location_set_version_id,
                            year_start, year_end,
                            by_sex = 1, by_age = 1,
                            custom_sex_id = NULL, custom_age_group_id = NULL) {

  ## Make Frame
  df <- get_demographics(location_set_version_id, year_start, year_end, by_sex, by_age, custom_sex_id, custom_age_group_id)
  for (ids in c("location_id", "year_id", "age_group_id", "sex_id")) {
    assign(ids, paste0(unique(df[[ids]]), collapse = ","))
  }

  ## Pull
  dbname <- "shared"
  host <- "modeling-cod-db.ihme.washington.edu"
  query <- paste0("SELECT 
                  o.age_group_id,
                  year_id,
                  o.location_id,
                  o.sex_id,
                  pop_scaled 
                  FROM
                  mortality.output o
                  LEFT JOIN
                  mortality.output_version ov using (output_version_id)
                  LEFT JOIN
                  shared.age_group a using (age_group_id)
                  LEFT JOIN
                  shared.location l using (location_id)
                  LEFT JOIN
                  shared.sex s using (sex_id)
                  WHERE
                  ov.is_best = 1
                  and year_id in (", year_id, ")
                  and o.location_id in (", location_id, ")
                  and o.sex_id in (", sex_id, ")
                  and o.age_group_id in (", age_group_id, ")")
  run_query(dbname, host, query)
}
