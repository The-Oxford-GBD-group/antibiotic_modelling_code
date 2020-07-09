# format covariate string
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: covariates
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname format_covariates
#' @export
format_covariates <- function(covariates = covariates) {
  if (length(covariates) == 1) {
    # split back into parts
    covariates <- unlist(tstrsplit(covariates, "\\+"))
  }
  covariates <- trimws(covariates)
  return(covariates)
}
