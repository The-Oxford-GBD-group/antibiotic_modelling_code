#' @title Get fitted model parameters from TMB object
#' @description Make a nice table of fitted model parameters from a geostat TMB object
#' @author Roy Burstein
#'
#' @param model_fit fitted TMB object that comes out of fit_mbg_tmb()
#' @param exp_fixed_effects Boolean, should the fixed effects be exponentiated (if model was fit with logit link). Defaults to TRUE
#' @param transform_hyperparams Boolean, should hyperparmeters be transformed from fitted to more natural space. Defaults to TRUE
#' @param draws Integer, number of draws to use. Defaults to 1000
#' @param calculate_range Boolean, should we calculate and report range using kappa in draws space. Defaults to TRUE
#' @param calculate_nom_var  Boolean, should we calculate and report nominal variance using kappa and tau in draws space. Defaults to TRUE
#'
#' @return formatted data.table object
#' @export
fitted_param_table_tmb <- function(model_fit,
                                   exp_fixed_effects     = TRUE,
                                   transform_hyperparams = TRUE,
                                   draws                 = 1000,
                                   calculate_range       = TRUE,
                                   calculate_nom_var     = TRUE,
                                   cov_constraints = covariate_constraint_vectorize(config)) {
  
  # get draws of parameter values
  mu <- model_fit$sdrep$par.fixed
  pm <- model_fit$sdrep$jointPrecision[1:length(mu),1:length(mu)]
  draws <- rmvnorm_prec(mu,pm,draws)

  # deal with given names of parameters
  fn <- names(model_fit$sdrep$par.fixed)
  fn[fn == 'alpha_j'] <- model_fit$fenames

  # Apply constraint transformations
  tmb_const <- tmb_cov_constraint(model_fit$fenames, cov_constraints)
  draws[names(model_fit$sdrep$par.fixed) == "alpha_j",] <-
    apply_constraints(tmb_const, draws[names(model_fit$sdrep$par.fixed) == "alpha_j",])

  # Transform fixed effects
  if(exp_fixed_effects == TRUE){
    draws[which(fn %in% model_fit$fenames),] <- exp(draws[which(fn %in% model_fit$fenames),])
  }

  # Get the range parameter
  if(calculate_range == TRUE){
    # see equation 6.16 (pg 196) of Blangiardo and Cameletti Book 
    ranger <- sqrt(8) / exp(draws[which(fn == 'logkappa'),])
    draws  <- rbind(draws, ranger)
    fn     <- c(fn, 'range')
  }
  
  # Get the nominal variance parameter
  if(calculate_nom_var == TRUE){
    # see equation 6.17 (pg 196) of Blangiardo and Cameletti Book 
    nominal_variance <- 1 / (4 * pi * (exp(draws[which(fn == 'logkappa'),]))^2 * (exp(draws[which(fn == 'logtau'),]))^2)
    draws <- rbind(draws, nominal_variance)
    fn    <- c(fn, 'nominal_variance')
  }
  
  # transform hyperparmeters
  if(transform_hyperparams == TRUE){
    draws[which(fn == 'logtau'),]             <- exp(draws[which(fn == 'logtau'),])
    draws[which(fn == 'logkappa'),]           <- exp(draws[which(fn == 'logkappa'),])
    draws[which(fn == 'log_nugget_sigma'),]   <- exp(draws[which(fn == 'log_nugget_sigma'),])
    draws[which(fn == 'log_cre_sigma'),]      <- exp(draws[which(fn == 'log_cre_sigma'),])
    draws[which(fn == 'log_nidre_sigma'),]    <- exp(draws[which(fn == 'log_nidre_sigma'),])
    
    fn[fn == 'logtau']           <- 'tau'
    fn[fn == 'logkappa']         <- 'kappa'
    fn[fn == 'log_nugget_sigma'] <- 'nugget_SD'
    fn[fn == 'log_cre_sigma']    <- 'country_RE_SD'
    fn[fn == 'log_nidre_sigma']  <- 'NID_RE_SD'
    
    draws[which(fn == 'zrho'),] <- (exp( draws[which(fn == 'zrho'),] ) - 1) / (exp( draws[which(fn == 'zrho'),] ) + 1)
    draws[which(fn == 'trho'),] <- (exp( draws[which(fn == 'trho'),] ) - 1) / (exp( draws[which(fn == 'trho'),] ) + 1)
    fn[fn == 'zrho'] <- 'age_rho'
    fn[fn == 'trho'] <- 'year_rho'
    
  }
  
  # summarize draws and clean up data table
  su <- data.table(t(apply(draws,1,quantile,c(0.025,0.500,0.975))))
  su[, fn := fn]
  colnames(su) <- c('lower','median','upper','param_name')
  su <- su[,c('param_name','median','lower','upper'),with=FALSE]
  
  # return the final table
  return(su)
}
