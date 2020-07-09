#' @title Modify constraints to fit TMB formatting
#' 
#' @author Neal Marquez
#' 
#' @description Optionally add constraints to fixed effects in the TMB 
#'   optimization model. In the TMB model, a constraint label of '0' indicates
#'   that the fixed effect is unconstrained, a label of '1' constrains the 
#'   above zero, and and a label of '-1' constrains the variable below zero.
#'
#' @param fes character, fixed effects in model
#' @param constraints named int vector output from the 
#'   `covariate_constraint_vectorize()` function. Specifies how to constrain 
#'   each fixed effect.
#' @param zl int, additional constraints to pad on which will be 0
#' 
tmb_cov_constraint <- function(
  model_fes,
  constraints = covariate_constraint_vectorize(config)
  ){
  tmb_const <- sapply(unlist(strsplit(model_fes, " \\+ ")), function(m){
    if(m %in% names(constraints)){
      x <- unname(constraints[m])
    }
    else{
      x <- 0
    }
    x
  })

  return(tmb_const)
}
