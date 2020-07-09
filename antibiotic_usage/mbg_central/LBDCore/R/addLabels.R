#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rate PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param country PARAM_DESCRIPTION
#' @param countries PARAM_DESCRIPTION, Default: NULL
#' @param col PARAM_DESCRIPTION, Default: grey(0.7)
#' @param gap PARAM_DESCRIPTION, Default: diff(range(rate))/60
#' @param cex PARAM_DESCRIPTION, Default: 0.7
#' @param adj PARAM_DESCRIPTION, Default: 0
#' @param xpd PARAM_DESCRIPTION, Default: NA
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[plotrix]{spreadout}}
#' @rdname addLabels
#' @export
#' @importFrom plotrix spreadout
addLabels <- function(rate,
                      year,
                      country,
                      countries = NULL,
                      col = grey(0.7),
                      gap = diff(range(rate)) / 60,
                      cex = 0.7,
                      adj = 0,
                      xpd = NA,
                      ...) {

  # add country names on RHS
  # arguments as before, with gap to define spacing between labels.
  # dots are passed to text

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

  # keep only those for latest year
  max_year <- max(as.numeric(year))
  year_idx <- which(year == max_year)
  rate <- rate[year_idx]
  year <- year[year_idx]
  country <- country[year_idx]

  # keep only those for countries requires
  ctry_idx <- which(country %in% countries)
  rate <- rate[ctry_idx]
  year <- year[ctry_idx]
  country <- country[ctry_idx]

  # order by countries
  ctry_o <- match(countries, country)
  rate <- rate[ctry_o]
  year <- year[ctry_o]
  country <- country[ctry_o]

  # get a good gap
  y_pos <- plotrix::spreadout(rate, gap)
  text(
    x = max_year + 1,
    y = y_pos,
    labels = country,
    col = col,
    cex = cex,
    adj = adj,
    xpd = xpd,
    ...
  )
}
