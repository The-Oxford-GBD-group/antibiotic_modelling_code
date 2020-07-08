#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path_to_parent_str PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_id
#' @export
get_id <- function(path_to_parent_str) {
  get_ihme_lc_id(loc_id_helper, path_to_parent_str) # nolint
}
