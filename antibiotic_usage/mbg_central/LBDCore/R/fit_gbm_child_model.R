#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_name PARAM_DESCRIPTION, Default: 'gbm'
#' @param fold_id_col PARAM_DESCRIPTION, Default: 'fold_id'
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param tc PARAM_DESCRIPTION, Default: 4
#' @param lr PARAM_DESCRIPTION, Default: 0.005
#' @param bf PARAM_DESCRIPTION, Default: 0.75
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
#' @rdname fit_gbm_child_model
#' @export
fit_gbm_child_model <- function(df, model_name = "gbm", fold_id_col = "fold_id", covariates = all_fixed_effects, additional_terms = NULL, weight_column = NULL,
                                tc = 4, lr = 0.005, bf = 0.75, indicator = indicator, indicator_family = indicator_family, cores = "auto") {


  # a function to fit the GBMs for stacking
  # basically a wrapper function for the fit_gam function (which is its own wrapper function-- hooray for rabbit holes)
  # model_name = what do you want the full fit model to be called upon the return. Must sync with subsquent functions

  # prevent df scoping
  df <- copy(df)

  # fit the baby trees in parallel
  folds <- unique(df[, get(fold_id_col)])

  message("Fitting baby gbm models in parallel")
  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  # Determine appropriate number of cores to use in `mclapply()`
  if (cores == "auto") cores <- get_max_forked_threads(nobjs = length(folds))
  baby_models <- mclapply(folds, function(fff)
    fit_gbm(
      df = df[get(fold_id_col) != fff, ],
      covariates = covariates,
      additional_terms = additional_terms,
      weight_column = weight_column,
      tc = tc,
      lr = lr,
      bf = bf,
      indicator = indicator,
      indicator_family = indicator_family,
      plot.main = F
    ), mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  for (fff in folds) {
    # use the data fit on K-1 of the folds to fit on the help out fold
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_models[[fff]], df[get(fold_id_col) == fff, ], n.trees = baby_models[[fff]]$gbm.call$best.trees, type = "response")]
  }


  # fit GBM
  message("Fitting Full GBM")
  full_model <- fit_gbm(
    df = df,
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    tc = tc,
    lr = lr,
    bf = bf,
    indicator = indicator,
    indicator_family = indicator_family
  )


  # add a model name slot
  full_model$model_name <- model_name

  # predict the main BRT
  df[, paste0(model_name, "_full_pred") := predict(full_model, df, n.trees = full_model$gbm.call$best.trees, type = "response")]

  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)
  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
