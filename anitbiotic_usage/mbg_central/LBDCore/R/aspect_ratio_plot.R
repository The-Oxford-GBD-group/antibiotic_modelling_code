#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plt PARAM_DESCRIPTION
#' @param aspect_ratio PARAM_DESCRIPTION, Default: 1
#' @param expand PARAM_DESCRIPTION, Default: 0.05
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname aspect_ratio_plot
#' @export
aspect_ratio_plot <- function(plt, aspect_ratio = 1, expand = 0.05) {
  # Function to fix aspect ratios

  xlims <- ggplot_build(plt)$layout$panel_scales$x[[1]]$range$range
  ylims <- ggplot_build(plt)$layout$panel_scales$y[[1]]$range$range

  # Try a different way if null - varies by ggplot build
  if (is.null(xlims)) xlims <- ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range
  if (is.null(ylims)) ylims <- ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range

  xmid <- mean(xlims)
  xrange <- xlims[2] - xlims[1]
  ymid <- mean(ylims)
  yrange <- ylims[2] - ylims[1]

  if (xrange / yrange < aspect_ratio) {
    new_yrange <- yrange * (1 + expand)
    new_xrange <- new_yrange * aspect_ratio
  }

  if (xrange / yrange > aspect_ratio) {
    new_xrange <- xrange * (1 + expand)
    new_yrange <- new_xrange / aspect_ratio
  }

  new_xlim <- c(xmid - 0.5 * new_xrange, xmid + 0.5 * new_xrange)
  new_ylim <- c(ymid - 0.5 * new_yrange, ymid + 0.5 * new_yrange)

  return(plt + expand_limits(
    x = new_xlim,
    y = new_ylim
  ))
}
