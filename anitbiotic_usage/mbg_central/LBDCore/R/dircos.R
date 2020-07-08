#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x1 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname dircos
#' @export
dircos <- function(x1) {
  coslat1 <- cos((x1[, 2] * pi) / 180)
  sinlat1 <- sin((x1[, 2] * pi) / 180)
  coslon1 <- cos((x1[, 1] * pi) / 180)
  sinlon1 <- sin((x1[, 1] * pi) / 180)
  cbind(coslon1 * coslat1, sinlon1 * coslat1, sinlat1)
}
