#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_name PARAM_DESCRIPTION, Default: 'earth'
#' @param fold_id_col PARAM_DESCRIPTION, Default: 'fold_id'
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param cores PARAM_DESCRIPTION, Default: 'auto'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_earth_child_model
#' @export
fit_earth_child_model <- function(df, model_name = "earth", fold_id_col = "fold_id", covariates = all_fixed_effects,
                                  additional_terms = NULL, weight_column = NULL, indicator = indicator, indicator_family = "binomial", cores = "auto") {

  # fit_earth_child_model: a function to fit a multivariate adaptive regression splines, with the kfold crossval
  # df: data table with the outcome/indicator and some covariates already extracted. This is different than the gam_cov functions
  # model_name: model name
  # fold_id_col: What column identifies the folds
  # covariates: a  vector of covariate names and/or formula in the style of all_fixed_effects/right hand side of a formula (e.g. cov1 + cov2 + cov3)
  # additional terms: a vector or single character of column names to be included in the model fit
  # weight_column: in df, is there a column that specifies the observation weights?
  # indicator: name of the column of the DV
  # indicator_family: model family
  # cores: # of cores are available for use

  # remove scoping surprises
  df <- copy(df)

  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # start by fitting the full gam
  message("Fitting the Full earth model")
  full_model <- fit_earth(
    df = df,
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    indicator = indicator,
    indicator_family = indicator_family
  )

  # add a name to the game object
  full_model$model_name <- model_name

  # fit the child/cv gams
  message("Fitting baby earth")
  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  # Determine appropriate number of cores to use in `mclapply()`
  if (cores == "auto") cores <- get_max_forked_threads(nobjs = length(folds))
  baby_models <- mclapply(folds, function(fff) fit_earth(
      df = df[get(fold_id_col) != fff, ],
      covariates = covariates,
      additional_terms = additional_terms,
      weight_column = weight_column,
      indicator = indicator,
      indicator_family = indicator_family
    ), mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  for (fff in folds) {
    # use the data fit on K-1 of the folds to fit on the help out fold
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_models[[fff]], df[get(fold_id_col) == fff, the_covs, with = F], type = "response")]
  }

  # predict using full model fit earlier
  df[, paste0(model_name, "_full_pred") := predict(full_model, df[, the_covs, with = F], type = "response")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)
  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
