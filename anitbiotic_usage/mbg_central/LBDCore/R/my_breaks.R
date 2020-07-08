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
#' @rdname my_breaks
#' @export
my_breaks <- function(x) {
  breaks <- c(min(x), mean(x), max(x))

  # Add mid_value if was originally manually specified
  if (add_mid_value == T) {
    breaks <- c(breaks, mid_value)
  }

  names(breaks) <- attr(breaks, "labels")
  breaks
}
