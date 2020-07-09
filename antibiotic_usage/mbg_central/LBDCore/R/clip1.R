#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xylim.clip PARAM_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @param upper PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname clip1
#' @export
clip1 <- function(xylim.clip, i, upper) {
  if (upper) {
    xylim.clip[1, i] <- max(q$threshold, xylim.clip[1, i])
  } else {
    xylim.clip[2, i] <- min(q$threshold, xylim.clip[2, i])
  }
  xylim.clip
}
