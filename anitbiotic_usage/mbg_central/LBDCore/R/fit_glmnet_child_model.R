#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_name PARAM_DESCRIPTION, Default: 'glmnet'
#' @param fold_id_col PARAM_DESCRIPTION, Default: 'fold_id'
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param alpha PARAM_DESCRIPTION, Default: 1
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
#' @rdname fit_glmnet_child_model
#' @export
fit_glmnet_child_model <- function(df, model_name = "glmnet", fold_id_col = "fold_id", covariates = all_fixed_effects, additional_terms = NULL, weight_column = NULL, alpha = 1, indicator = indicator, indicator_family = "binomial", cores = "auto") {

  # Fit a glmnet child model
  # df: the data in data table format
  # model_name: what do you want to call this?
  # covariates: formula of the fixed effects
  # fold_id_col: what is the column iding the fold
  # additional_terms: constants, other covarites. Usually only used for year and other non-raster covariates.
  # indicator_family: analyitical family
  # indicator: what indicator
  # parallel: TRUE/FALSE to turn on/off parallelization
  # alpha: paramter for glmnet


  df <- copy(df)
  message("Fitting the Full GLMNET")

  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # glmnet has it's own internal parallelization that we don't have much control
  # over. It is either on or off and is parallel over each fold supplied by the
  # 'nfolds' argument to `cv.glmnet()`. Since we use the default of 10 for
  # 'nfolds' in `cv.glmnet()` in the `fit_glmnet()` function (i.e. we don't pass
  # in an 'nfolds' argument), we have to make sure we at least have 10 cores to
  # use before we turn the parallel option on. See this:
  # https://www.rdocumentation.org/packages/glmnet/versions/2.0-16/topics/cv.glmnet

  # Also, `cv.glmnet()` has caused multithreaded operations to hang similar to
  # `mclapply()`, so we set multithreaded operations to serial here
  set_serial_threads()
  if (cores == "auto") cores <- get_total_threads()
  if (cores >= 10) {
    parallel <- TRUE
    # start the cluster if parallel
    # Apparently, with Linux systems we don't need the `cl <- makeCluster(cores)`
    # step. Doing it that way will also work, but it is much slower.
    # Do it here, so we don't have to spin up/down the cluster with each
    # `fit_glmnet()` call.
    registerDoParallel(cores = cores)
  } else {
    parallel <- FALSE
  }

  # fit the full model
  full_model <- fit_glmnet(df,
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    alpha = alpha,
    indicator = indicator,
    indicator_family = indicator_family,
    parallel = parallel
  )

  full_model$model_name <- model_name

  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  for (fff in folds) {
    # message(paste0('Fitting and Predicting Fold: ', fff))
    baby_model <- fit_glmnet(df[get(fold_id_col) != fff, ],
      covariates = covariates,
      additional_terms = additional_terms,
      weight_column = weight_column,
      alpha = alpha,
      indicator = indicator,
      indicator_family = indicator_family,
      parallel = parallel
    )

    new_data <- df[get(fold_id_col) == fff, the_covs, with = F]

    n_nd <- names(new_data)
    new_data <- as.matrix(new_data)
    names(new_data) <- n_nd

    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_model, newx = new_data, s = baby_model$cv_1se_lambda, type = "link")]
  }

  # stop the cluster just in case
  stopImplicitCluster()

  # Return to multithreading (if any):
  set_original_threads()

  # predict using full model fit earlier
  new_data <- df[, the_covs, with = F]

  n_nd <- names(new_data)
  new_data <- as.matrix(new_data)
  names(new_data) <- n_nd
  df[, paste0(model_name, "_full_pred") := predict(full_model, newx = new_data, s = full_model$cv_1se_lambda, type = "link")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)

  # if binomial, undo the logit
  if (indicator_family == "binomial") {
    df[, return_cols[1] := invlogit(get(return_cols[1]))]
    df[, return_cols[2] := invlogit(get(return_cols[2]))]
  }


  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
