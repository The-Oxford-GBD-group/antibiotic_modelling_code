#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_covariate_version
#' @export
get_covariate_version <- function(list) {
  dbname <- "shared"
  host <- "modeling-covariates-db.ihme.washington.edu"

  query <- paste0("SELECT
                  data_version.covariate_id,
                  covariate_name_short,
                  model_version.data_version_id,
                  model_version.model_version_id,
                  model_version.last_updated,
                  model_version.description 			
                  FROM covariate.model_version 
                  JOIN covariate.data_version ON model_version.data_version_id=data_version.data_version_id
                  JOIN shared.covariate ON data_version.covariate_id=covariate.covariate_id
                  WHERE covariate.last_updated_action!='DELETE' 
                  AND is_best=1 AND LOWER(covariate_name_short) in (", tolower(toString(shQuote(list))), ")")

  run_query(dbname, host, query)
}
