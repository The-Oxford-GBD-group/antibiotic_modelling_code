#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pred.rast PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make.2000.2015.aroc
#' @export
make.2000.2015.aroc <- function(pred.rast) {
  ## ############################
  ## make.2000.2015.aroc
  ## old way of making AROC
  ## ###########################


  aroc.rast <- pred.rast[[1]]
  values(aroc.rast) <- (log(values(pred.rast[[16]])) - log(values(pred.rast[[1]]))) / 15

  return(aroc.rast)
}
