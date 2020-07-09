#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ras PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fetch_covariate_layer
#' @export
fetch_covariate_layer <- function(ras, period = 1) {
  # fetch covariate layer
  # given a raster-like object and a period, returns the appropriate layer, assuming chronological order

  if (class(ras) == "RasterBrick" | class(ras) == "RasterStack") {
    return(ras[[period]])
  } else {
    return(ras)
  }
}
