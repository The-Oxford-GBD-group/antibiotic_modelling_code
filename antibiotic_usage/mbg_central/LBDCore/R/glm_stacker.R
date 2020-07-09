#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_names PARAM_DESCRIPTION, Default: c("gam", "gbm")
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_family PARAM_DESCRIPTION, Default: indicator_family
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname glm_stacker
#' @export
glm_stacker <- function(df, # the dataset in data table
                        model_names = c("gam", "gbm"), # prefixes of the models to be stacked
                        indicator = indicator, # the indicator of analysis
                        indicator_family = indicator_family) { # indicator family (e.g. binomial)

  # A function to stack the estimates from child modules using GLM
  # df: dataset. It must have the *_cv_pred and *_full_pred columns from the child models
  # indicator: name of the outcome/indicator column in the dataset
  # indicator_family: family that the stacker should be fit with
  # NOTE: This function is somewhat out of date. Use the gam stacker instead for now.

  # copy dataset to avoid weird data table scoping issues
  df <- copy(df)

  # format the outcome variable depending on the family
  if (indicator_family == "binomial") {
    df[, failures := N - get(indicator)] # failures in the sense they didn't get sick
    outcome <- df[, .(get(indicator), failures)]
    names(outcome)[1] <- indicator
  } else {
    outcome <- df[, .(get(indicator))]
    names(outcome)[1] <- indicator
  }

  outcome <- as.matrix(outcome)

  # format the child model results into the glm format
  # create new columns to hold the cv results, which are then replaced with the full results upon prediction
  df[, (model_names) := lapply(model_names, function(mn) get(paste0(mn, "_cv_pred")))]

  # collapse the model names to a basic formula
  glm_formula <- as.formula(paste0("outcome~", paste(model_names, collapse = "+")))
  stacker <- glm(glm_formula, family = indicator_family, data = df)

  # predict the results as fit from the crossvalidated stuff
  df[, stacked_cv_pred := predict(stacker, df, type = "response")]

  # overwrite the columns to work on the full fit child modules
  df[, (model_names) := lapply(model_names, function(mn) get(paste0(mn, "_full_pred")))]
  df[, stacked_pred := predict(stacker, df, type = "response")]

  # return the dataframe and the stacker model
  return(setNames(list(df[, stacked_pred], stacker), c("dataset", "stacker_model"))) # [,.(stacked_pred)]
}
