#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getPointsWrapper
#' @export
getPointsWrapper <- function(x) {
  poly_name <- paste(pars[x, c("shapefile", "location_code")], collapse = "__")
  poly <- polys[[poly_name]]

  ## NOTE: simply resampling on 2010 population data for now for convenience. Could redo by year, but wouldnt make much of difference anyway.
  if (is.null(poly)) {
    ## make a dummy, empty points dataframe
    points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
  } else {
    points <- try(
      getPoints(
        shape = poly,
        raster = popraster[[3]],
        n = density,
        perpixel = perpixel,
        prob = prob
      )
    )
    if (inherits(points, "try-error")) {
      points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
    } else {
      colnames(points) <- c("longitude", "latitude", "weight")
    }
  }
  return(points)
}
