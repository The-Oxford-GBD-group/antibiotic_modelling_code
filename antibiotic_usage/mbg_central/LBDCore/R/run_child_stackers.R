#' @title Fitting child stackers
#' @description
#' Parallelization in stackers is a little bit complicated. Some of the stackers
#' rely on packages like "mgcv" and "glmnet" which have their own internal
#' parallelization which we have to make sure plays nice with the MKL.
#' \code{mclapply()} is used heavily in stackers as well which will hang with
#' multithreaded operations like OpenMP or MKL.

#' Those stacking functions which have a "cores" argument have an "auto" option
#' (default) where the stacker function has code to properly determine how many
#' cores to give each operation based on how it is parallelized, how many cores
#' are available for the job based on slots, etc. It is highly recommended that
#' the "auto" option be used. Otherwise, you may pass in an integer argument for
#' the number of cores. The only value that is known to work generally on all
#' stackers is 1, or serial operation.
#'
#' @param models a vector of sub-models to run
#' @param input_data the data frame with the input data, defaulted to \code{the_data} from the global environment
#' @param indicator Indicator name e.g. "tr_had_diarrhea"
#' @param indicator_family Model family (e.g. "binomial")
#' @param covariates_nonbrt Covariates used in non-BRT child models
#' @param covariates_brt Covariates used in BRT (GBM or XGBOOST) child models
#' @param outputdir Output directory
#' @param reg Region
#'
#' @return A list of sub-model predictions and model objects where the first element of each list member are the predictions,
#' and the second element the model statistics and summary
#'
#' @rdname run_child_stackers
#'
#' @importFrom raster extent crop mask
#'
#' @export
run_child_stackers <- function(models,
                               input_data,
                               indicator = indicator,
                               indicator_family  = "binomial",
                               covariates_nonbrt = all_fixed_effects,
                               covariates_brt = all_fixed_effects_brt,
                               outputdir = outputdir,
                               reg = reg) {
  if ("gam" %in% models) {
    tic("Stacking - GAM")
    gam_child <- fit_gam_child_model(
      df = input_data,
      model_name = "gam",
      fold_id_col = "fold_id",
      covariates = covariates_nonbrt,
      additional_terms = NULL,
      weight_column = "weight",
      bam = FALSE,
      spline_args = list(bs = "ts", k = 3),
      auto_model_select = TRUE,
      indicator = indicator,
      indicator_family = indicator_family,
      cores = "auto"
    )

    toc(log = T)
  }

  # Fit a GBM/BRT model
  if ("gbm" %in% models) {
    tic("Stacking - GBM")
    gbm_child <- fit_gbm_child_model(
      df = input_data,
      model_name = "gbm",
      fold_id_col = "fold_id",
      covariates = covariates_brt,
      weight_column = "weight",
      tc = as.numeric(gbm_tc),
      lr = as.numeric(gbm_lr),
      bf = as.numeric(gbm_bf),
      indicator = indicator,
      indicator_family = indicator_family,
      cores = "auto"
    )
    toc(log = T)
  }

  # Fit a BRT model with Xgboost (faster than GBM)
  if ("xgboost" %in% models) {
    tic("Stacking - Xgboost")

    if (!exists("xg_model_tune")) {
      message("xg_model_tune not found in config. Will be set to true, and model will be tuned with default grid search")
      xg_model_tune <- TRUE
      hyperparameter_filepath <- NULL
    }

    if (xg_model_tune == T & !exists("hyperparameter_filepath")) {
      message("Tuning xgboost on default grid search")
      hyperparameter_filepath <- NULL
    }
    xgboost_child <- fit_xgboost_child_model(
      df = input_data,
      covariates = covariates_brt,
      weight_column = "weight",
      indicator = indicator,
      indicator_family = indicator_family,
      outputdir = outputdir,
      region = reg,
      xg_model_tune = xg_model_tune,
      hyperparameter_filepath = hyperparameter_filepath
    )
    toc(log = T)
  }

  # fit some nets
  # lasso
  if ("lasso" %in% models) {
    tic("Stacking - lasso")
    lasso_child <- fit_glmnet_child_model(
      df = input_data,
      model_name = "lasso",
      covariates = covariates_nonbrt,
      fold_id_col = "fold_id",
      additional_terms = NULL,
      indicator_family = indicator_family,
      indicator = indicator,
      alpha = 1,
      weight_column = "weight",
      cores = "auto"
    )
    toc(log = T)
  }

  # ridge
  if ("ridge" %in% models) {
    tic("Stacking - ridge")
    ridge_child <- fit_glmnet_child_model(
      df = input_data,
      model_name = "ridge",
      covariates = covariates_nonbrt,
      fold_id_col = "fold_id",
      additional_terms = NULL,
      indicator_family = indicator_family,
      indicator = indicator,
      alpha = 0,
      weight_column = "weight",
      cores = "auto"
    )
    toc(log = T)
  }

  # enet
  if ("enet" %in% models) {
    tic("Stacking - enet")
    enet_child <- fit_glmnet_child_model(
      df = input_data,
      model_name = "enet",
      covariates = covariates_nonbrt,
      fold_id_col = "fold_id",
      additional_terms = NULL,
      indicator_family = indicator_family,
      indicator = indicator,
      alpha = 0.5,
      weight_column = "weight",
      cores = "auto"
    )
    toc(log = T)
  }

  ## Get all child ones
  child_models <- lapply(paste0(models, "_child"), function(x) get(x))

  ## Return the list of child stacker (pred and model)
  return(child_models)
}
