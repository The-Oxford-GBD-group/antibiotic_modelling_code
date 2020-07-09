#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plot_data PARAM_DESCRIPTION
#' @param multiple_runs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot_overlay
#' @export
plot_overlay <- function(plot_data, multiple_runs) {

  # Plot overlay defines where each object (ggplot and title) are placed on the pdf.
  # This depends on if data is being plotted, and if multiple runs are used
  if (plot_data == T) {
    if (multiple_runs == T) {
      lay <- rbind(
        c(1, 1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 1, 4, 4, 4, NA),
        c(1, 1, 1, 1, 1, 4, 4, 4, NA),
        c(1, 1, 1, 1, 1, 4, 4, 4, 5)
      )
    } else {
      lay <- rbind(
        c(1, 1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 1, 2, 2, 3, 3),
        c(1, 1, 1, 1, 1, 4, 4, 4, 4),
        c(1, 1, 1, 1, 1, 4, 4, 4, 4),
        c(1, 1, 1, 1, 1, 4, 4, 4, 4)
      )
    }
  } else {
    lay <- rbind(
      c(1, 1, 1, 1, 2, 2, 3, 3),
      c(1, 1, 1, 1, 2, 2, 3, 3),
      c(1, 1, 1, 1, 4, 4, 4, 4),
      c(1, 1, 1, 1, 4, 4, 4, 4),
      c(1, 1, 1, 1, 4, 4, 4, 4),
      c(1, 1, 1, 1, NA, NA, NA, 5)
    )
  }
  return(lay)
}
