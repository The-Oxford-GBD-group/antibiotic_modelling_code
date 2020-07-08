#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xylim_clip PARAM_DESCRIPTION
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
#' @rdname clip2
#' @export
clip2 <- function(xylim_clip, i, upper) {
  if (upper) {
    xylim_clip[1, i] <- max(q$threshold, xylim_clip[1, i])
  } else {
    xylim_clip[2, i] <- min(q$threshold, xylim_clip[2, i])
  }
  xylim_clip
}
