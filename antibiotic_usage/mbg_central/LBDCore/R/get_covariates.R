#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list PARAM_DESCRIPTION
#' @param ci PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_covariates
#' @export
get_covariates <- function(list, ci = FALSE) {

  ## Get metadata
  meta <- get_covariate_metadata(list)

  ## Parse into age/sex specific, age specific, sex specific
  meta <- data.table(meta)
  age_sex <- meta[by_sex == 1 & by_age == 1, .(covariate_name_short)]
  age <- meta[by_sex == 0 & by_age == 1, .(covariate_name_short)]
  sex <- meta[by_sex == 1 & by_age == 0, .(covariate_name_short)]
  all <- meta[by_sex == 0 & by_age == 0, .(covariate_name_short)]

  ## Reorder from the most specific to most general
  list <- unique(rbind(age_sex, age, sex, all)$covariate_name_short)

  # Pull and Merge Covariates
  flag <- 0
  output <- NULL
  for (cov in list) {
    data <- pull_covariate(cov, ci)

    if (flag == 0) {
      output <- data
      flag <- 1
    } else {
      output <- age_sex_merge(output, data)
    }
  }

  return(output)
}
