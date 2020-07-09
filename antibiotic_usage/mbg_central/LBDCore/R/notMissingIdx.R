#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param raster PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details Taken from seegSDM (https://github.com/SEEG-Oxford/seegSDM/blob/master/R/seegSDM.R)
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname notMissingIdx
#' @export
notMissingIdx <- function(raster) {
  # return an index for the non-missing cells in raster
  which(!is.na(getValues(raster)))
}
