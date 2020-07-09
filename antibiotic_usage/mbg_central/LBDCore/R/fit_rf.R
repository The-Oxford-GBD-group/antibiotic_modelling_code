
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param ntree PARAM_DESCRIPTION, Default: 1000
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_rf
#' @export
fit_rf <- function(df, covariates = all_fixed_effects, additional_terms = NULL, ntree = 1000, indicator, indicator_family = "binomial") {
  # fit random forests
  df <- copy(df)

  # add additional terms if requested
  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # random forest doesn't have a binomial or possion option. Use emperical logit (this also keeps it consistent with other methods that return logit probabilities)
  # create outcome variable
  if (indicator_family == "binomial" | indicator_family == "poisson") {
    # message('emplogit')
    df[, y := emplogit(get(indicator), N)]
    # message(names(df))
  } else {
    df[, y := get(indicator)]
  }


  # fit a random forest
  message(paste0("Fitting Random Forest with ntree: ", ntree))
  model <- randomForest(x = df[, the_covs, with = F], y = df[, y], ntree = ntree)

  return(model)
}
