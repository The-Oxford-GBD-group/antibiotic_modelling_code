
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param start.rast PARAM_DESCRIPTION
#' @param aroc.rast PARAM_DESCRIPTION
#' @param n.yrs PARAM_DESCRIPTION, Default: 15
#' @param uselogit PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make.forecast
#' @export
make.forecast <- function(start.rast, aroc.rast, n.yrs = 15, uselogit = FALSE) {

  ## ###################################################################
  ## make.forecast()
  ##
  ## forecast forwards in time some raster of your choosing
  ##
  ## INPUT:
  ##
  ##   start.rast: raster to start the forecast with
  ##
  ##   aroc.rast: raster whose pixels are AROC
  ##
  ##   n.yrs: how many years to forecast
  ##
  ##   uselogit: if TRUE, convert to logitspace before forecasting and inverts back to make final maps
  ##
  ##
  ## OUTPUT:
  ##
  ## a forecasted raster object
  ## ###################################################################

  ## we use p*e^{r*t}

  fc.rast <- start.rast

  if (uselogit) fc.rast <- log(fc.rast / (1 - fc.rast))

  values(fc.rast) <- values(start.rast) * exp(values(aroc.rast) * n.yrs)

  if (uselogit) fc.rast <- exp(fc.rast) / (1 + fc.rast)

  return(fc.rast)
}
