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
#' @rdname getPointsWrapper.w.dict
#' @export
getPointsWrapper.w.dict <- function(x) {
  which.rast <- as.character(x[2])
  x <- as.numeric(x[1])
  poly_name <- paste(pars[x, c("shapefile", "location_code")], collapse = "__")
  poly <- polys[[poly_name]]

  ## NOTE: simply resampling on 2010 population data for now for convenience. Could redo by year, but wouldnt make much of difference anyway.
  if (is.null(poly)) {
    ## make a dummy, empty points dataframe
    points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
  } else {
    rast <-
      points <- try(
        getPoints(
          shape = poly,
          raster = (if (length(raster_list[[which.rast]]) == 1) raster_list[[which.rast]][[1]] else raster_list[[which.rast]][[1]]),
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
