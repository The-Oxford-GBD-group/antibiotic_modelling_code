#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rate PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param ylab PARAM_DESCRIPTION, Default: ''
#' @param title PARAM_DESCRIPTION, Default: ''
#' @param xlim PARAM_DESCRIPTION, Default: c(1999, 2016)
#' @param ylim PARAM_DESCRIPTION, Default: c(0, max(rate))
#' @param line_years PARAM_DESCRIPTION, Default: c(2000, 2005, 2010, 2015)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname prepLines
#' @export
prepLines <- function(rate,
                      year,
                      ylab = "",
                      title = "",
                      xlim = c(1999, 2016),
                      ylim = c(0, max(rate)),
                      line_years = c(2000, 2005, 2010, 2015)) {
  # set up the base plot for the custom line chart

  plot(rate ~ year,
    type = "n",
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = xlim,
    ylim = ylim
  )

  for (ly in line_years) {
    lines(
      x = rep(ly, 2),
      y = ylim,
      lwd = 3,
      lty = 3,
      col = grey(0.8)
    )
  }

  axis(1,
    lty = 0,
    col.axis = grey(0.4),
    line = -1
  )

  axis(2,
    las = 2,
    cex.axis = 0.8,
    col = grey(0.4),
    col.axis = grey(0.4)
  )

  title(
    main = title,
    col.main = grey(0.35),
    cex.main = 1.2,
    line = 0.5
  )

  title(
    ylab = ylab,
    col.lab = grey(0.4),
    cex.lab = 1.2
  )
}
