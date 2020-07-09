#' @title Multiple ggplots
#' @description Function to plot multiple ggplot objects together#'
#' @source Adapted from \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#'
#' @param ... Can pass in ggplot objects in \code{...} or in plotlist
#' @param plotlist Can pass in ggplot objects as a list with this argument instead of in \code{...}
#' @param cols Number of columns in layout
#' @param layout Matrix specifying the layout; i.e. \code{matrix(c(1,2,3,3), nrow = 2, byrow = T)}.
#'               If \code{layout} is specified, then \code{cols} is ignored
#' @param legend A legend object.  If legend is passed, then this will add an extra cell at the
#'               end of the grid layout and insert the legend there (good, for instance, if you
#'               have common legends for all of your plots and only want to show it once).
#' @return Prints a gridded output of your ggplot objects to the active graphical device
#' @note gg_stackers_results is a list of ggplot objects
#'
#' @examples
#' \dontrun{
#' # Use first legend only
#' the_legend <- g_legend(gg_stackers_results[[1]])
#' gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position = "none"))) #'
#' multiplot(
#'   plotlist = gg_stackers_results,
#'   cols = ceiling(length(gg_stackers_results) / 2),
#'   legend = the_legend
#' )
#' }
#' @export
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, legend = NULL) {



  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)
  if (!is.null(legend)) numPlots <- numPlots + 1 # add legend

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
      ncol = cols, nrow = ceiling(numPlots / cols),
      byrow = T
    )
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:(numPlots - 1)) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      ))
    }

    # Print the legend if present
    if (!is.null(legend)) {
      matchidx <- as.data.frame(which(layout == numPlots, arr.ind = T))
      legend$vp <- viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      )
      grid.draw(legend)
    }
  }
}
