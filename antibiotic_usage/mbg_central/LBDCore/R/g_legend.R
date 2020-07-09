#' @title Pulls legend out of a ggplot object for use later
#'
#' @description Pulls legend out of a ggplot object for use later, based off of function originally written by Hadley Wickham
#'
#' @param a.gplot ggplot() object
#' @return grob containing legend
#' @examples
#' \dontrun{
#' the_legend <- g_legend(my_gplot)
#' }
#' @export
g_legend <- function(a.gplot) {
  pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
  } else {
    legend <- NULL
  }
  graphics.off()
  return(legend)
}
