#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_name PARAM_DESCRIPTION, Default: 'rf'
#' @param fold_id_col PARAM_DESCRIPTION, Default: 'fold_id'
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param ntree PARAM_DESCRIPTION, Default: 1000
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_family PARAM_DESCRIPTION, Default: indicator_family
#' @param cores PARAM_DESCRIPTION, Default: 'auto'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_rf_child_model
#' @export
fit_rf_child_model <- function(df, model_name = "rf", fold_id_col = "fold_id", covariates = all_fixed_effects, additional_terms = NULL, ntree = 1000, indicator = indicator, indicator_family = indicator_family, cores = "auto") {
  # fit a random forest model for stacking
  # df: data table
  # model_name: what do you want to call this?
  # covariates: formula of the fixed effects
  # fold_id_col: what is the column iding the fold
  # additional_terms: constants, other covarites. Usually only used for year and other non-raster covariates.
  # indicator_family: analyitical family
  # indicator: what indicator
  # cores: how many cores can be used?

  df <- copy(df)
  message("Fitting the Full Random Forest")

  # format covariate string
  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  full_model <- fit_rf(df,
    covariates = covariates,
    additional_terms = additional_terms,
    ntree = ntree,
    indicator = indicator,
    indicator_family = indicator_family
  )

  full_model$model_name <- model_name

  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  message("Fitting baby forests in parallel")
  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  # Determine appropriate number of cores to use in `mclapply()`
  if (cores == "auto") cores <- get_max_forked_threads(nobjs = length(folds))
  # fit all the baby forests at once
  baby_models <- mclapply(folds, function(fff) fit_rf(df[get(fold_id_col) != fff, ],
      covariates = covariates,
      additional_terms = additional_terms,
      ntree = ntree,
      indicator = indicator,
      indicator_family = indicator_family
    ), mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()
  # predict iteratively for simplification. I don't think RF likes changing variable names
  for (fff in folds) {
    # message(paste0('Predicting Fold: ', fff))
    # sub_data = pred_filler[get(fold_id_col)==fff,]
    # le_preds = predict(baby_forests[[fff]], newdata = sub_data, type = 'response')
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_models[[fff]], newdata = df[get(fold_id_col) == fff, the_covs, with = F], type = "response")]
  }

  # predict using full model fit earlier
  df[, paste0(model_name, "_full_pred") := predict(full_model, df[, the_covs, with = F], type = "response")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)

  # if binomial, take the invlogit
  if (indicator_family == "binomial") {
    df[, return_cols[1] := invlogit(get(return_cols[1]))]
    df[, return_cols[2] := invlogit(get(return_cols[2]))]
  }

  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
