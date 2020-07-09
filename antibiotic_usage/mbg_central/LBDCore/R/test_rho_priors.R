#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param temporal_model_theta1_prior PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname test_rho_priors
#' @export
test_rho_priors <- function(temporal_model_theta1_prior) {
  temporal_model_theta1_prior <- eval(parse(text = temporal_model_theta1_prior))
  mean <- temporal_model_theta1_prior$param[1]
  prec <- temporal_model_theta1_prior$param[2]
  sd <- sqrt(1 / prec)

  message(paste0("theta1_prior_prec: ", round(prec, 2)))
  lower <- transform_theta(mean - (1.96 * sd))
  upper <- transform_theta(mean + (1.96 * sd))
  message(paste0("95% range in Rho prior: ", round(transform_theta(mean), 2), " (", round(lower, 2), " - ", round(upper, 2), ")"))
}
