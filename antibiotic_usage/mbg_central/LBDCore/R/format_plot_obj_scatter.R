#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param map PARAM_DESCRIPTION
#' @param scatter PARAM_DESCRIPTION
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
format_plot_obj_scatter <- function(map, scatter) {
  legend <- g_legend(scatter)
  scatter <- scatter + guides(color = F, size = F)

  lay <- rbind(
    c(1, 1, 1, 1, 2, 2),
    c(1, 1, 1, 1, 2, 2),
    c(1, 1, 1, 1, 3, 3),
    c(1, 1, 1, 1, 3, 3)
  )

  return(arrangeGrob(scatter, legend, map, layout_matrix = lay))
}
