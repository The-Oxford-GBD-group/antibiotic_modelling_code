#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param geo PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname splitGeoNames
#' @export
splitGeoNames <- function(geo) {
  # split country/years (in format 'country_year') out of rownames
  # of a geostatistical conditional simulation object and add as columns
  splits <- strsplit(rownames(geo), split = "_")
  ctry <- sapply(splits, "[", 1)
  year <- sapply(splits, "[", 2)
  geo <- data.frame(
    iso3 = ctry,
    year = year,
    geo,
    stringsAsFactors = FALSE
  )
  rownames(geo) <- NULL
  return(geo)
}
