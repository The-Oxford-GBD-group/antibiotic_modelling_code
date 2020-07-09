#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gbdval PARAM_DESCRIPTION
#' @param pixelval PARAM_DESCRIPTION
#' @param weightval PARAM_DESCRIPTION
#' @param MaxIter PARAM_DESCRIPTION, Default: 40
#' @param MaxJump PARAM_DESCRIPTION, Default: 10
#' @param FunTol PARAM_DESCRIPTION, Default: 1e-05
#' @param approx_0_1 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname LogitFindK
#' @export
LogitFindK <- function(gbdval, pixelval, weightval, MaxIter = 40, MaxJump = 10, FunTol = 1e-5, approx_0_1) {

  # Logit raking won't work with any values of 0 or 1 in cell_pred
  # Adjust values slightly to avoid -Inf or Inf in NewPixelVal
  if (approx_0_1) {
    pixelval[pixelval == 0] <- 1e-10
    pixelval[pixelval == 1] <- 1 - (1e-10)
  }

  NumIter <- ceiling(-log2(FunTol / MaxJump))

  if (NumIter > MaxIter) {
    stop(paste("Maximum number of iterations is less than projected iterations required:", NumIter / MaxIter))
  }

  CurrentError <- EvalDiff(gbdval, pixelval, weightval)
  if (CurrentError > 0) {
    Range <- c(0, MaxJump)
  } else {
    Range <- c(-MaxJump, 0)
  }

  a <- Range[1]
  b <- Range[2]
  F_a <- EvalDiff(gbdval, NewPixelVal(a, pixelval), weightval)
  F_b <- EvalDiff(gbdval, NewPixelVal(b, pixelval), weightval)

  if (F_a * F_b > 0) {
    stop("Your estimates are WAY too far away from GBD")
  } else {
    i <- 1
    c <- (a + b) / 2
    F_c <- EvalDiff(gbdval, NewPixelVal(c, pixelval), weightval)
    Success <- (abs(F_c) <= FunTol)
    while (!Success & i < NumIter) {
      if (sign(F_c) == sign(F_a)) {
        a <- c
        F_a <- F_c
      } else {
        b <- c
        F_b <- F_c
      }
      c <- (a + b) / 2
      F_c <- EvalDiff(gbdval, NewPixelVal(c, pixelval), weightval)
      Success <- (abs(F_c) <= FunTol)
      i <- i + 1
    }
    if (Success) {
      return(c)
    } else {
      return(sprintf("Need more iterations, current output: K = %g, F(K) = %g", c, F_c))
    }
  }
}
