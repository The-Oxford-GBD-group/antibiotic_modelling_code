#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cov PARAM_DESCRIPTION
#' @param ci PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname pull_covariate
#' @export
pull_covariate <- function(cov, ci = FALSE) {
  dbname <- "shared"
  host <- "modeling-covariates-db.ihme.washington.edu"

  if (ci) ci_query <- paste0(", model.lower_value AS '", tolower(paste0(cov, "_lower")), "',
                              model.upper_value AS '", tolower(paste0(cov, "_upper")), "'")
  if (!ci) ci_query <- ""

  query <- paste0(
    "SELECT
                  model.location_id,
                  model.year_id,
                  model.age_group_id,
                  model.sex_id,
                  model.mean_value AS '", tolower(cov), "'",
    ci_query,
    "FROM covariate.model
                  JOIN covariate.model_version ON model.model_version_id=model_version.model_version_id
                  JOIN covariate.data_version ON model_version.data_version_id=data_version.data_version_id
                  JOIN shared.covariate ON data_version.covariate_id=covariate.covariate_id
                  JOIN shared.location ON model.location_id=location.location_id
                  JOIN shared.age_group ON model.age_group_id=age_group.age_group_id
                  WHERE covariate.last_updated_action!='DELETE' 
                  AND is_best=1 AND covariate.covariate_name_short = '", cov, "'"
  )
  data <- run_query(dbname, host, query)

  if (!(4749 %in% unique(data$location_id)) | !(44533 %in% unique(data$location_id))) data <- janky_covariate_fix(data, cov)

  return(data)
}
