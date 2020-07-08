#' @title FUNCTION_TITLE
#' @description unction to update the cov layers option
#' @param original_layers PARAM_DESCRIPTION
#' @param new_layers PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname update_cov_layers
#' @export
update_cov_layers <- function(original_layers, new_layers) {
  return(append(original_layers, new_layers))
}
