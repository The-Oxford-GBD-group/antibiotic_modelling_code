# get predictions from predictive INLA glm models
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param result PARAM_DESCRIPTION
#' @param intercept PARAM_DESCRIPTION, Default: '(Intercept)'
#' @param fixed_continuous PARAM_DESCRIPTION, Default: NULL
#' @param fixed_group PARAM_DESCRIPTION, Default: NULL
#' @param random_group PARAM_DESCRIPTION, Default: 'cluster_id'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname predGLM
#' @export
predGLM <- function(result,
                    intercept = "(Intercept)",
                    fixed_continuous = NULL,
                    fixed_group = NULL,
                    random_group = "cluster_id") {

  # starting means and variances
  pred_mean <- pred_var <- 0

  # add intercept terms
  if (!is.null(intercept)) {
    pred_mean <- pred_mean + result$summary.fixed[intercept, "mean"]
    pred_var <- pred_var + result$summary.fixed[intercept, "sd"]^2
  }

  # add continuous fixed effects terms
  if (!is.null(fixed_continuous)) {
    for (name in fixed_continuous) {

      # get coefficients
      coef_mean <- result$summary.fixed[name, "mean"]
      coef_var <- result$summary.fixed[name, "sd"]^2

      # get covariate
      cov <- result$model.matrix[, name]

      pred_mean <- pred_mean + cov * coef_mean
      pred_var <- pred_var + (cov^2) * coef_var
    }
  }

  # add discrete fixed effects terms
  if (!is.null(fixed_group)) {
    for (name in fixed_group) {

      # find group members
      idx <- grep(sprintf("^%s*", name), rownames(result$summary.fixed))

      # get clean names
      names <- rownames(result$summary.fixed)[idx]
      names_clean <- gsub(name, "", names)

      # get coefficients
      coef_mean <- result$summary.fixed[idx, "mean"]
      coef_var <- result$summary.fixed[idx, "sd"]^2

      # get covariate
      cov <- result$model.matrix[, names]

      pred_mean <- pred_mean + as.vector(cov %*% coef_mean)
      pred_var <- pred_var + as.vector((cov^2) %*% coef_var)
    }
  }

  # add discrete random effects terms
  if (!is.null(random_group)) {
    for (name in random_group) {

      # get coefficients
      coef_level <- result$summary.random[[name]][, "ID"]
      coef_mean <- result$summary.random[[name]][, "mean"]
      coef_var <- result$summary.random[[name]][, "sd"]^2

      # get covariate
      cov <- result$.args$data[, name]

      # match up the levels
      length(cov)
      length(coef_level)
      idx <- match(cov, coef_level)

      pred_mean <- pred_mean + coef_mean[idx]
      pred_var <- pred_var + coef_var[idx]
    }
  }

  # return result
  ans <- data.frame(
    mean = pred_mean,
    sd = sqrt(pred_var)
  )

  return(ans)
}
