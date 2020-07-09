#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param covariate_name_short PARAM_DESCRIPTION
#' @param filters PARAM_DESCRIPTION, Default: list()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[RMySQL]{MySQLDriver-class}}
#' @rdname get_cov_estimates
#' @export
#' @importFrom RMySQL MySQL
get_cov_estimates <- function(covariate_name_short, filters = list()) {

  # Setup base query
  base_query <- sprintf("
                        SELECT
                        model.model_version_id,
                        covariate.covariate_id,
                        covariate.covariate_name_short,
                        model.location_id,
                        location.location_name,
                        model.year_id,
                        model.age_group_id,
                        age_group.age_group_name,
                        model.sex_id,
                        model.mean_value,
                        model.lower_value,
                        model.upper_value
                        FROM covariate.model
                        JOIN covariate.model_version ON model.model_version_id=model_version.model_version_id
                        JOIN covariate.data_version ON model_version.data_version_id=data_version.data_version_id
                        JOIN shared.covariate ON data_version.covariate_id=covariate.covariate_id
                        JOIN shared.location ON model.location_id=location.location_id
                        JOIN shared.age_group ON model.age_group_id=age_group.age_group_id
                        WHERE covariate_name_short='%s'", covariate_name_short)

  query_params <- c()
  # Default to best model if not specified
  if (!("model_version_id" %in% names(filters))) {
    query_params <- c(query_params, "AND is_best=1")
  }

  # Implement additional filters
  for (f in names(filters)) {
    fvalues <- paste(unlist(filters[f]), collapse = ",")

    # Fix table ambiguity for known join columns
    if (f %in% c("age_group_id", "model_version_id", "data_version_id", "location_id")) {
      f <- paste0("model.", f)
    }

    ext_param <- sprintf("AND %s IN (%s)", f, fvalues)
    query_params <- c(query_params, ext_param)
  }

  # Construct filtered query
  query <- c(base_query, query_params)
  query <- paste(query, collapse = " ")

  # Get and return estimates
  conn <- dbConnect(RMySQL::MySQL(), host = "modeling-covariates-db.ihme.washington.edu", username = "readonly", password = "justlooking")
  dbSendQuery(conn, "SET NAMES utf8")
  estimates <- dbGetQuery(conn, query)
  dbDisconnect(conn)

  Encoding(estimates$location_name) <- "UTF-8"

  return(estimates)
}
