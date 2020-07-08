#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_name PARAM_DESCRIPTION, Default: 'gam'
#' @param fold_id_col PARAM_DESCRIPTION, Default: 'fold_id'
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param bam PARAM_DESCRIPTION, Default: F
#' @param spline_args PARAM_DESCRIPTION, Default: list(bs = "ts", k = 3)
#' @param auto_model_select PARAM_DESCRIPTION, Default: T
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
#' @rdname fit_gam_child_model
#' @export
fit_gam_child_model <- function(df, model_name = "gam", fold_id_col = "fold_id", covariates = all_fixed_effects,
                                additional_terms = NULL, weight_column = NULL, bam = F, spline_args = list(bs = "ts", k = 3),
                                auto_model_select = T, indicator = indicator, indicator_family = "binomial", cores = "auto") {

  # a wrapper function for fitting gam/bam models in the stacking framework. It'll run 1+k times internally
  # arguments are the same as fit gam above.
  # basically a wrapper function for the fit_gam function (which is its own wrapper function-- hooray for rabbit holes)

  # remove scoping surprises
  df <- copy(df)

  # start by fitting the full gam
  message("Fitting the Full GAM model")
  full_model <- fit_gam(df,
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    bam = bam,
    spline_args = spline_args,
    auto_model_select = auto_model_select,
    indicator = indicator,
    indicator_family = indicator_family,
    cores = cores
  )

  # add a name to the game object
  full_model$model_name <- model_name

  # fit the child/cv gams
  message("Fitting baby gams")
  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  for (fff in folds) {
    baby_model <- fit_gam(
      df = df[get(fold_id_col) != fff, ],
      covariates = covariates,
      additional_terms = additional_terms,
      bam = bam,
      weight_column = weight_column,
      spline_args = spline_args,
      auto_model_select = auto_model_select,
      indicator = indicator,
      indicator_family = indicator_family,
      cores = cores
    )

    # fill in the data
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_model, df[get(fold_id_col) == fff, ], type = "response")]
  }

  # predict using full model fit earlier
  df[, paste0(model_name, "_full_pred") := predict(full_model, df, type = "response")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)
  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
