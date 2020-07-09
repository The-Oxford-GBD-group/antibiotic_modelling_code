#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @param period_end PARAM_DESCRIPTION
#' @param width PARAM_DESCRIPTION, Default: 60
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname targetYear
#' @export
targetYear <- function(period, period_end, width = 60) {
  # get the target year, based on the period, period size (in months) and end
  # date of the final period

  # convert date to cmc
  period_end <- Date2cmc(period_end)

  # get number of months preceeding
  months <- width / 2 + width * (period - 1)

  # subtract
  cmc <- period_end - months + 1

  # format as a year
  ans <- toYear(cmc2Date(cmc))

  return(ans)
}
