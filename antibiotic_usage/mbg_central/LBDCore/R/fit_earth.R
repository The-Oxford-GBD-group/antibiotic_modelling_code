#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
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
#' @rdname fit_earth
#' @importFrom earth earth
#' @export
fit_earth <- function(df, covariates = all_fixed_effects, additional_terms = NULL, weight_column = NULL, indicator, indicator_family = "binomial") {


  # fit earth: a function to fit a multivariate adaptive regression splines
  # df: data table with the outcome/indicator and some covariates already extracted. This is different than the gam_cov functions
  # covariates: a  vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
  # additional terms: a vector or single character of column names to be included in the model fit
  # weight_column: in df, is there a column that specifies the observation weights?
  # indicator: name of the column of the DV
  # indicator_family: model family

  df <- copy(df) # in case data table scoping gets wonky

  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # set response variable
  if (indicator_family == "binomial") response <- cbind(success = df[, get(indicator)], failure = df[, N] - df[, get(indicator)])
  if (indicator_family == "gaussian") response <- cbind(outcome = df[, get(indicator)])

  # sort out weights
  # format weights
  if (!is.null(weight_column)) {
    df[, data_weight := get(weight_column)]
  } else {
    df[, data_weight := 1]
  }
  weight_column <- "data_weight"

  # fit the earth
  message(paste0("Fitting earth"))

  model <- earth(x = df[, the_covs, with = F], y = response, weights = df[, get(weight_column)], glm = list(family = indicator_family))

  # return the earth object
  return(model)
}
