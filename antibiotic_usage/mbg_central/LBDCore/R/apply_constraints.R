#' @title Apply constraints to fitted value draws
#' 
#' @author Neal Marquez
#' 
#' @description Given draws of fixed effect values generated from a fitted TMB
#'   model and a list of constraints applied to fixed effects in that model, 
#'   apply transformations on constrained fixed effects to reproduce how they
#'   were incorporated in the model (constraining them above or below zero). If
#'   a fixed effect had a constraint label of 0 (unconstrained, the default),
#'   the untransformed draws will be returned.
#' 
#' @param tmb_const int vector, tmb prepped constraints
#' @param alpha_draws matrix, fitted draws of coefficients
#' 
#' @return matrix of transformed beta coefficients for fixed effect draws
#' 
apply_constraints <- function(tmb_const, FE_draws){
  Nfe <- nrow(FE_draws)
  
  FEdraws_const <- sapply(1:Nfe, function(j){
    X <- FE_draws[j,]
    if(tmb_const[j] == 1){
      X <- exp(X)
    }
    else if(tmb_const[j] == -1){
      X <- -exp(X)
    }
    
    return(X)
  })
  
  return(t(FEdraws_const))
}
