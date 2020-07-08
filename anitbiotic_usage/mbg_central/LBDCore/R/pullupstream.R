#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname pullupstream
#' @export
pullupstream <- function() {
  system("cd /share/code/geospatial/lbd_core\ngit pull origin master")
}
