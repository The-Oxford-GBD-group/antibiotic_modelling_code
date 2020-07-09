#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fes PARAM_DESCRIPTION
#' @param add_terms PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname add_additional_terms
#' @export
add_additional_terms <- function(fes, add_terms = NULL) {
  new_list <- paste(unlist(sapply(c(fes, add_terms), function(x) format_covariates(x))), collapse = " + ")
  return(new_list)
}
