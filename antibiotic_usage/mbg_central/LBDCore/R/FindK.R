#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p_i PARAM_DESCRIPTION
#' @param p_N PARAM_DESCRIPTION
#' @param N_i PARAM_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @param Mult PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname FindK
#' @export
FindK <- function(p_i, p_N, N_i, a, Mult = FALSE) {

  ## p_i = our grid of probabilities
  ## p_N = national target
  ## sim_N_i = our grid of sample sizes (population raster)
  ## a = bounds for raking factor, perhaps return warning if optimal is on edge of limits

  if (Mult == TRUE) {
    Limits <- c(0, a)
  } else {
    Limits <- c(-a, a)
  }
  iter <- 1
  Boundary <- TRUE
  while (Boundary & iter < 10) {
    Limits <- Limits * iter
    val <- optimize(EvalK, Limits, p_i = p_i, p_N = p_N, N_i = N_i, tol = 1e-20)$min
    Boundary <- (round(abs(val)) == Limits[2])
    iter <- iter + 1
  }
  return(val)
}
