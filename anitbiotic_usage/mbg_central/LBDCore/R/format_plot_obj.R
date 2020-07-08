#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param plot_obj PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname format_plot_obj
#' @export
format_plot_obj <- function(plot_obj) {
  legend <- g_legend(plot_obj$scatter)
  scatter <- plot_obj$scatter + guides(color = F, size = F)
  map <- plot_obj$map

  lay <- rbind(
    c(1, 1, 1, 1, 2, 2),
    c(1, 1, 1, 1, 2, 2),
    c(1, 1, 1, 1, 3, 3),
    c(1, 1, 1, 1, 3, 3)
  )

  return(grid.arrange(scatter, legend, map, layout_matrix = lay))
}
