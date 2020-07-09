#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param location_set_version_id PARAM_DESCRIPTION
#' @param year_start PARAM_DESCRIPTION
#' @param year_end PARAM_DESCRIPTION
#' @param by_sex PARAM_DESCRIPTION, Default: 1
#' @param by_age PARAM_DESCRIPTION, Default: 1
#' @param custom_sex_id PARAM_DESCRIPTION, Default: NULL
#' @param custom_age_group_id PARAM_DESCRIPTION, Default: NULL
#' @param covariates PARAM_DESCRIPTION, Default: NULL
#' @param population PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_square
#' @export
make_square <- function(location_set_version_id,
                        year_start, year_end,
                        by_sex = 1, by_age = 1,
                        custom_sex_id = NULL, custom_age_group_id = NULL,
                        covariates = NULL, population = FALSE) {
  ## Skeleton
  df <- get_demographics(
    location_set_version_id,
    year_start, year_end,
    by_sex, by_age,
    custom_sex_id, custom_age_group_id
  )

  ## Covariates
  if (!is.null(covariates)) {
    covs <- get_covariates(covariates)
    df <- age_sex_merge(df, covs)
  }

  ## Population
  if (population) {
    pops <- get_populations(location_set_version_id, year_start, year_end, by_sex, by_age)
    df <- age_sex_merge(df, pops)
  }

  return(df)
}
