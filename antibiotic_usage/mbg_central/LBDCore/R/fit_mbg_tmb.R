#' @title Fit a TMB MBG model
#' @description Fit a TMB MBG model and pull jointPrecision report
#' @author Roy Burstein
#'
#' @param lbdcorerepo core repo location
#' @param cpp_template name of cpp template file within ./<lbdcorerepo>/mbg_central/
#' @param tmb_input_stack object that results from build_mbg_data_stack_tmb() or build_mbg_data_stack(...,tmb=TRUE)
#' @param ADmap_list map parameter for ignoring parameters
#' @param control_list pass control list to nlminb()
#' @param optimizer which software to use for optimization (optim, or nlminb)
#'
#' @return list of tmb model objects: ADfun, opt, sdrep
#'
#' # @useDynLib mbg_tmb_model
#'
#' @note TODO support for other data models, sum to one constraint, country random effects
#' @export
fit_mbg_tmb <- function(lbdcorerepo     = core_repo,
                        cpp_template    = 'mbg_tmb_model',
                        tmb_input_stack = input_data,
                        ADmap_list      = NULL,
                        control_list    = NULL,
                        optimizer       = 'nlminb',
                        sparse_ordering  = TRUE
                        ){
  
  message('WARNING: This TMB implementation does not support sum to one constraints yet.')
  
  # make sure TMB is loaded
  require(TMB)
  
  # compile the cpp file and dynload it
  message('compiling template')
  TMB::compile(sprintf('%s/mbg_central/%s.cpp', lbdcorerepo, cpp_template))
  dyn.load( dynlib(sprintf('%s/mbg_central/%s', lbdcorerepo, cpp_template)) )
  
  # deal with parallelization
  threads <- system('echo $OMP_NUM_THREADS', intern = TRUE)
  if(threads != '') {
    message(sprintf('Detected %s threads in OMP environmental variable.',threads))
    openmp(as.numeric(threads))
  } else {
    message('Did not detect environmental OMP variable, defaulting to 4 cores. \n
             You can set this using OMP_NUM_TREADS or when launching singularity image.')
    openmp(4)
  }
  
  # set Data flag for Kaspers normalization fix
  tmb_input_stack$Data$flag <- 1 
  
  # if no z-col then set that ar1 to null
  if(length(dim(tmb_input_stack$Parameters$Epsilon_stz))==2){
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['zrho']] <- factor(NA)
  }
  
  # REs
  randompars <- c("Epsilon_stz")
  
  # if use_gp is on (options[6]==1) Epsilon_stz should be random, otherwise map GP stuff out
  if(tmb_input_stack$Data$options$useGP==1){
    #randompars <- c(randompars,"Epsilon_stz")
  } else { 
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    for(par in c('logtau','logkappa','trho','zrho'))
      ADmap_list[[par]] <- factor(NA)
   }
    
  
  # if nugget option is on add nug_i to randompars
  if(tmb_input_stack$Data$options$nugget==1){
    randompars <- c(randompars,'nug_i')
  } else { # if no nugget, add nug related parameters to ignorelist
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['log_nugget_sigma']] <- factor(NA)
    ADmap_list[['nug_i']]            <- rep(factor(NA),length(tmb_input_stack$Parameters$nug_i))
  }
  
  # if country RE option is on add cntry_re to randompars
  if(tmb_input_stack$Data$options$country_random==1) {
    randompars <- c(randompars,'cntry_re')
  } else { # if no nugget, add nug related parameters to ignorelist
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['log_cre_sigma']]    <- factor(NA)
    ADmap_list[['cntry_re']]         <- rep(factor(NA),length(tmb_input_stack$Parameters$cntry_re))
  }

  # if NID RE option is on add nid_re to randompars
  if(tmb_input_stack$Data$options$NID_random==1) {
    randompars <- c(randompars,'nid_re')
  } else { # if no nugget, add nug related parameters to ignorelist
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['log_nidre_sigma']]    <- factor(NA)
    ADmap_list[['nid_re']]         <- rep(factor(NA),length(tmb_input_stack$Parameters$nid_re))
  }
  
  if(tmb_input_stack$Data$options$GEO_random==1) {
    randompars <- c(randompars,'geo_re')
  } else { # if no nugget, add nug related parameters to ignorelist
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['log_geore_sigma']]    <- factor(NA)
    ADmap_list[['geo_re']]         <- rep(factor(NA),length(tmb_input_stack$Parameters$geo_re))
  }
  
  # map out model sigma if no gaussian observations
  if(sum(tmb_input_stack$Data$lik_gaussian_i) == 0){
    if(is.null(ADmap_list)) 
      ADmap_list <- list()
    ADmap_list[['log_gauss_sigma']]    <- factor(NA) 
  }
  
  
  # print
  message(paste0('ADMAP_LIST: ',  paste0(names(ADmap_list),collapse=', ')))
  message(paste0('Random Pars: ', paste0(randompars,collapse=', ')))
  
  
  # make the AD object
  message('Making AD object')
  obj <- MakeADFun(data       = tmb_input_stack$Data, 
                   parameters = tmb_input_stack$Parameters,  
                   map        = ADmap_list, 
                   random     = randompars, 
                   hessian    = TRUE, 
                   DLL        = cpp_template) #sprintf('%s/mbg_central/%s', lbdcorerepo, tmpl))
  
  # normalize
  obj <- normalize(obj, flag = "flag")
  
  # Reduce fill in of sparse Cholesky factorization (requires metis install of TMB)
  if(sparse_ordering){
    runSymbolicAnalysis(obj)
  }

  # Run optimizer
  message('Running MLE')
  if(optimizer == 'nlminb')
    opt0 <- do.call("nlminb",list(start       =    obj$par,
                                  objective   =    obj$fn,
                                  gradient    =    obj$gr,
                                  lower       =    tmb_input_stack$L, 
                                  upper       =    tmb_input_stack$U, 
                                  control     =    control_list))
  if(optimizer == 'optim')
    opt0 <- do.call("optim",list(par = obj$par, fn = obj$fn, control = control_list, gr = obj$gr, method = 'BFGS'))
  
  # run sdreport to get joint precision of all parameters
  for(i in 1:20) message('Getting Joint Precision')
  SD0 <- TMB::sdreport(obj, getJointPrecision = TRUE, bias.correct = TRUE)

  # return
  return( list ( ADfun   = obj,
                 opt     = opt0,
                 sdrep   = SD0,
                 fenames = colnames(tmb_input_stack$Data$X_ij)) )
  
}
