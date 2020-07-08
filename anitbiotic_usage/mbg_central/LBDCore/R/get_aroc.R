
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pred.rast PARAM_DESCRIPTION
#' @param year.map PARAM_DESCRIPTION, Default: NULL
#' @param pow PARAM_DESCRIPTION, Default: 1
#' @param uselogit PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get.aroc
#' @export
get.aroc <- function(pred.rast, year.map = NULL, pow = 1, uselogit = FALSE) {

  ## ######################################################################
  ## get.aroc()
  ##
  ## description: provides weighted aroc taken across years of prediction
  ##
  ## INPUT:
  ##
  ## pred.rast: takes a stacked raster object (raster brick), different
  ## layers are different years of prediction by default it assumes that
  ## spacing between layers is 1 year and that the top layer is the
  ## earliest year
  ##
  ## year.map: if the above default is not correct, this vector (of
  ## length == raster depth) defines which years are in the raster
  ## layers
  ##
  ## pow: exponential weight used in determining year_wt_i:
  ##      (yr_i - yr_1)/(yr_n - yr_i))^pow
  ## if pow==0, get uniform wts
  ## if pow==1, get linear wts
  ## if pow > 1, get exponential wts, ...
  ##
  ## uselogit: if set to TRUE, then it converts rasters to logit space before calculations
  ##
  ## OUTPUT: returns a raster of AROC where each pixel in the 1 layer
  ## raster the weighted AROC for that pixel determined from the
  ## pred.rast
  ## ######################################################################



  ## make the year.map if not supplied
  if (is.null(year.map)) {
    year.map <- 1:nlayers(pred.rast)
  }

  ## setup lengths of years and pixels
  n.yr <- length(year.map)
  n.px <- length(as.vector(pred.rast[[1]]))

  ## vectorize the pred.rast and make a matrix of RC between years
  pix.mat <- matrix(ncol = n.yr, nrow = n.px)

  ## convert to logit space?
  if (uselogit == TRUE) {
    pix.mat <- log(pix.mat / (1 - pix.mat))
  }

  ## calculate RC, substitute RC between yr_{i+1} and yr_i into pix.mat_i
  message("calculating rates of change")
  for (i in 1:(n.yr - 1)) {
    pix.mat[, i] <- log(values(pred.rast[[i + 1]])) - log(values(pred.rast[[i]])) / (year.map[i + 1] - year.map[i])
  }

  ## remove last column of matrix
  pix.mat <- pix.mat[, -n.yr]
  ## now we have a matrix of AROC between years for all pixels in pred.rast

  ## get unstandardized year weights
  year.wt <- (year.map - min(year.map))^pow
  ## leave out the earliest year which now has weight zero
  year.wt <- year.wt[-1]
  ## standardize to sum to 1
  year.wt <- year.wt / sum(year.wt)

  ## perform the linear combo of year wts and rate of changes by year
  ## to get weighted AROC pixel estimates
  message("making weighted rates of change")
  pix.aroc <- pix.mat %*% year.wt

  ## and put back into a raster
  message("preparing AROC raster")
  aroc.rast <- pred.rast[[1]]
  values(aroc.rast) <- pix.aroc

  return(aroc.rast)
}
