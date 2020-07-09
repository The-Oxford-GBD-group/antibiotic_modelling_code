#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param alpha PARAM_DESCRIPTION, Default: 1
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param parallel PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_glmnet
#' @importFrom glmnet glmnet cv.glmnet
#' @export
fit_glmnet <- function(df, covariates = all_fixed_effects, additional_terms = NULL, weight_column = NULL, alpha = 1, indicator, indicator_family = "binomial", parallel = FALSE) {

  # fit penalized regressions(lasso, elastic net, ridge)
  # df: the data, in data table format
  # covariates: string in formula notation denoting the covariates
  # additional_terms: other covariates to include
  # alpha: alpha parameter for glmnet calls
  # parallel: TRUE/FALSE to turn on/off parallelization
  # offset: is there an offset, valid only for poission models. Not fully implemented

  df <- copy(df)

  # add additional terms if requested
  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # format weights
  if (!is.null(weight_column)) {
    data_weights <- df[, get(weight_column)]
  } else {
    data_weights <- rep(1, nrow(df))
  }

  # create outcome object
  # specifying a binomial object is annoyingly difficult in glmnet. Use emplogit instead.
  if (indicator_family == "binomial") {
    not_indicator <- df[, N] - df[, get(indicator)]
    response_var <- cbind(not_indicator, outcome = df[, get(indicator)])
  } else if (indicator_family == "poisson") {
    if (!is.null(offset)) {
      stop("fit_glmnet does not currently work for offset poissons")
    } else {
      response_var <- df[, get(indicator)]
    }
  } else {
    response_var <- df[, get(indicator)]
  }


  # create design matrix
  dm <- as.matrix(df[, the_covs, with = F])
  colnames(dm) <- the_covs

  # search for lambda
  # these models are run as gaussian because of the prior transformation.
  message(paste0("Fitting glmnet with alpha: ", alpha))

  cv_res <- glmnet::cv.glmnet(x = dm, y = response_var, family = indicator_family, alpha = alpha, weights = data_weights, parallel = parallel)

  # fit full model using selected lambdas
  model <- glmnet::glmnet(x = dm, y = response_var, family = indicator_family, lambda = cv_res$lambda, alpha = alpha, weights = data_weights)

  # preserve the cv_1se_lambda
  model$cv_1se_lambda <- cv_res$lambda.1se

  return(model)
}
