#' @title Vectorize covariate constraints
#'
#' @description Creates a vector of constraints based on the MBG config
#'
#' @author Neal Marquez
#'
#' @param config data.frame, config data frame 
#'
#' @return vector of constraints with names for the covariates
#'
#'@export
covariate_constraint_vectorize <- function(config){
  fixed_effects <- 
    unlist(strsplit(unlist(config[unlist(config[,1]) == "fixed_effects",2]), " \\+ "))
  gbd_fixed_effects <- 
    unlist(strsplit(unlist(config[unlist(config[,1]) == "gbd_fixed_effects",2]), " \\+ "))
  fixed_effects_constraints <- 
    eval(parse(text=unlist(config[unlist(config[,1]) == "fixed_effects_constraints",2])))
  gbd_fixed_effects_constraints <- 
    eval(parse(text=unlist(config[unlist(config[,1]) == "gbd_fixed_effects_constraints",2])))
  
  if(is.null(gbd_fixed_effects_constraints) | length(gbd_fixed_effects_constraints) == 0){
    gbd_fixed_effects_constraints <- rep(0, length(gbd_fixed_effects))
  }
  else if(length(gbd_fixed_effects_constraints) == 1){
    gbd_fixed_effects_constraints <- rep(gbd_fixed_effects_constraints, length(gbd_fixed_effects))
  }
  
  if(is.null(fixed_effects_constraints) | length(fixed_effects_constraints) == 0){
    fixed_effects_constraints <- rep(0, length(fixed_effects))
  }
  else if(length(fixed_effects_constraints) == 1){
    fixed_effects_constraints <- rep(fixed_effects_constraints, length(fixed_effects))
  }
  
  constraints <- c(fixed_effects_constraints, gbd_fixed_effects_constraints)
  names(constraints) <- c(fixed_effects, gbd_fixed_effects)
  
  return(constraints)
}
