#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rate PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param country PARAM_DESCRIPTION
#' @param countries PARAM_DESCRIPTION, Default: NULL
#' @param col PARAM_DESCRIPTION, Default: grey(0.7)
#' @param size PARAM_DESCRIPTION, Default: 1
#' @param border PARAM_DESCRIPTION, Default: grey(0.4)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname addLines
#' @export
addLines <- function(rate,
                     year,
                     country,
                     countries = NULL,
                     col = grey(0.7),
                     size = 1,
                     border = grey(0.4)) {

  # given vectors: rate (y axis), year (x axis)
  # and country (grouping factor), make a nice line
  # plot with lollipop-like likes with border colour
  # 'border' and fill colour 'col' (repeated if length one)
  # If 'countries' is NULL, all countries are plotted,
  # otherwise only those named in this character vector

  # check inputs
  stopifnot(all.equal(
    length(rate),
    length(year),
    length(country)
  ))


  # sort countries
  all_countries <- sort(unique(country))
  if (is.null(countries)) {
    countries <- all_countries
  } else {
    stopifnot(all(countries %in% all_countries))
  }
  n_ctry <- length(countries)

  # expand col and bg if needed
  if (length(col) == 1) {
    col <- rep(col, n_ctry)
  } else {
    stopifnot(length(col) == n_ctry)
  }

  # loop through countries
  for (i in 1:n_ctry) {
    ctry <- countries[i]

    idx_ctry <- which(country == ctry)

    # dark grey outline
    lines(rate[idx_ctry] ~ year[idx_ctry],
      col = border,
      lwd = 7.5 * size
    )
    points(rate[idx_ctry] ~ year[idx_ctry],
      col = border,
      cex = 1 * size,
      pch = 16
    )

    # coloured foreground
    lines(rate[idx_ctry] ~ year[idx_ctry],
      col = col[i],
      lwd = 6 * size
    )

    points(rate[idx_ctry] ~ year[idx_ctry],
      col = col[i],
      pch = 16,
      cex = 0.85 * size
    )
  }
}
