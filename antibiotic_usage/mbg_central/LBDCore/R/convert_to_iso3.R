#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname convert_to_iso3
#' @export
convert_to_iso3 <- function(x) {
  if (nchar(x) > 3) x <- loc_names[loc_name == x, ihme_lc_id]
  return(x)
}
