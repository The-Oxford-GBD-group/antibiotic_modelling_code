#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param raster PARAM_DESCRIPTION
#' @param new_vals PARAM_DESCRIPTION
#' @param idx PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{brick}}
#' @rdname insertRaster
#' @export
#' @importFrom raster brick
insertRaster <- function(raster, new_vals, idx = NULL) {


  # calculate cell index if not provided
  if (is.null(idx)) idx <- cellIdx(raster)

  # check the index makes superficial sense
  stopifnot(length(idx) == nrow(new_vals))
  stopifnot(max(idx) <= ncell(raster))

  # create results raster
  n <- ncol(new_vals)
  raster_new <- raster::brick(replicate(n,
    raster[[1]],
    simplify = FALSE
  ))
  names(raster_new) <- colnames(new_vals)

  # update the values
  for (i in 1:n) {
    raster_new[[i]][idx] <- new_vals[, i]
  }

  return(raster_new)
}
