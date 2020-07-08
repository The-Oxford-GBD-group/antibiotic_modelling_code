#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param K PARAM_DESCRIPTION
#' @param p_i PARAM_DESCRIPTION
#' @param p_N PARAM_DESCRIPTION
#' @param N_i PARAM_DESCRIPTION
#' @param Mult PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname EvalK
#' @export
EvalK <- function(K, p_i, p_N, N_i, Mult = FALSE) {
  if (Mult == TRUE) {
    p_tilde <- G_Inv(G(p_i) * K)
  } else {
    p_tilde <- G_Inv(G(p_i) + K)
  }
  LHS <- Agg(p_tilde, N_i)
  RHS <- p_N
  SE <- (LHS - RHS)^2
  return(SE)
}
