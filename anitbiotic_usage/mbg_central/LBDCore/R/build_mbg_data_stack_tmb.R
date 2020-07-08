#' @title Build Stack for TMB MBG model
#' @description Organize Data and Parameter Stacks to fit a TMB MBG model
#' @author Roy Burstein
#'
#' @param d prepped model frame with no NAs
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of zcol for analysis (i.e. ages c(1,2,3,4,5). Must be integers starting with 1)
#' @param fes a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#'  corresponding to column names in d
#' @param indic indicator name, corresponding to the response variable in d
#' @param country_re TRUE/FALSE include country re. If true and there is a zcol then there will be random slope as well (todo)
#' @param nid_re TRUE/FALSE include re on survey. Need nid column in the data frame. defaults to use_nid_res from config. 
#' @param exclude_cs character vector of covariates to exclude from centrescaling
#' @param mesh an inla mesh object. NOTE! this gets remade inside this function... Roy said there was a good reason
#' @param s2mesh Logical. Should the mesh be created on the surface of
#'   a sphere? If TRUE, s2params is used to specify mesh parameters
#'   instead of max_edge and mesh_offset
#' @param s2params string of 3 element numeric vector in R
#'   notation. e.g. "c(25, 500, 1000)". The entries describe the
#'   minimum triangle edge length allowed, hos far to extend the mesh
#'   beyond the 'simple' boundary, and the maximum allowed triangle
#'   edge length, respectively. Units are in kilometers. Used only if
#'   s2mesh=TRUE.
#' @param cov_constraints named int vector. integer vector indexed by covariate names.
#'
#' @return returns a named list with Data and Parameters to be passed into fit_mbg_tmb()
#' 
#' TODO: Allow users to set priors, allow different data model (gaussian to start), country random effects
#' 
#' @export
build_mbg_data_stack_tmb <- function(d          = df,                 
                                     yl         = year_list,  
                                     zl         = z_list,
                                     fes        = all_fixed_effects,  
                                     indic      = indicator, 
                                     country_re = use_inla_country_res, 
                                     nid_re     = use_nid_res,
                                     geo_re     = use_geo_res,
                                     exclude_cs = '', 
                                     nugget     = FALSE,
                                     zcol       = NULL,
                                     shapefile_version = 'current', 
                                     scale_gaussian_variance_N = TRUE,
                                     mesh       = mesh_s,
                                     cov_constraints = covariate_constraint_vectorize(config)
                                     ){   
  
  # TODO incorporate an z_list like we do year_list istead of using max(zcol)

  # ensure d is a dt
  d <- setDT(d)
  
  # zcol
  if(!zcol %in% colnames(d)){
    message('No Z column detected')
    d[[zcol]] <- 0
  } else if (is.null(zcol)) {
    message('Z column was set as null')
    d[[zcol]] <- 0
  }
  if( !all(unique(d[[zcol]]) %in% zl)) {
    message('WARNING: zl and d[[zcol]] do not completely match up.. ')
    # check that we there arent values in zcol not matching z_list
    d[, dropz := !get(zcol) %in% zl]
    if(any(d$dropz != FALSE)){
      message(sprintf('WARNING: Detected some z values in zcol (%s) which were not in the z_list',zcol))
      message(sprintf('WARNING: Due to this, dropping %i rows from the input data',sum(d$dropz==TRUE)))
      print(table(d$dropz,d$age))
      d <- subset(d, dropz == FALSE)
      d[, dropz := NULL]
    }
  }
  
  # make a fake data point with no weight for the max period and Z to fill out the GP
  d <- rbind(d, d[1,])
  d[[zcol]][nrow(d)]     <- max(zl)
  d[['period']][nrow(d)] <- length(yl)
  d[['weight']][nrow(d)] <- 0
  

  # look for z dimension 
  num_z <- 1 # TODO get this from the z-list
  if(length(zl)>1){ 
    message(sprintf('More than one unique %s found, initiating Z in the GP', zcol))
    num_z <- length(zl)

    # set A proj grouping. The ordering here must match the ordering of epsilon_stz in the template
    grp <- setDT(expand.grid(1:length(yl), 1:max(zl)))
    setnames(grp,c('Var1','Var2'),c('period',zcol))
    grp[,group := 1:.N]
    d <- merge(d, grp, by = c('period',zcol), all.x = TRUE) # warning this may re-order things, so do not use d stuff from above
  } else {
    # set Aproj grouping to period if there is but one z value
    d$group <- d$period
  }
  
  # coordinates at data points. these are passed to TMB in long,lat so
  # we keep them and create another set for 3d coords
  coords   <- cbind(d$longitude,d$latitude)
  
  # remake mesh ## TODO? why remake?
  mesh   <- build_space_mesh(d           = d,
                             simple      = simple_polygon,
                             max_edge    = mesh_s_max_edge,
                             mesh_offset = mesh_s_offset,
                             s2mesh      = as.logical(use_s2_mesh),
                             s2params    = s2_mesh_params)
  
  # if we have  mesh on s2, first convert coords to spherical to project to mesh
  data.locs <- coords ## long, lat
  if(mesh$manifold == "S2"){
    ## then the mesh is on the sphere and we need to use 3d coords
    data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
  }

  # make a projection matrix from data to st mesh
  A.proj <- INLA::inla.spde.make.A(
    mesh = mesh,
    loc = data.locs,
    group = d$group
  )

  # Build SPDE object using INLA (must pass mesh$idx$loc when supplying Boundary) to get matrices
  spde <- INLA::inla.spde2.matern(mesh, alpha = 2)

  # make a clean design matrix. make sure all fes appear in d
  fes <- unlist(strsplit(fes, " \\+ "))
  if (!all(fes %in% names(d))) {
    stop("Check your fes argument, not all covariate names appear in d.")
  }
  spde <- inla.spde2.matern(mesh, alpha = 2, constr = gp_sum_to_zero) 
                                       
  # make a clean design matrix. make sure all fes appear in d
  fes  <- unlist(strsplit(fes, ' \\+ ')) # i dont think this is safe and we might want to consider changing to formulas
  if(!all(fes %in% names(d)))  
    stop('Check your fes argument, not all covariate names appear in d.')
  if(length(fes)!=0) {
    X_xp <- as.matrix(cbind(int=1, d[,c(fes),with=FALSE]))
  } else {
    X_xp <- as.matrix(cbind(int=rep(1,nrow(d))))
  }
  
  # add in age fixed effects
  # TODO eventually do country random slopes for each FE level of Z
  if(num_z > 1){ 
    message(sprintf('Adding fixed effects for levels of zcol (%s)',zcol))
    for(z in 2:num_z){
      X_xp <- cbind(X_xp, d[[zcol]] == z)
    } 
    colnames(X_xp) <- c(colnames(X_xp)[colnames(X_xp)!=''],paste0('FE_z_level__',2:num_z))
    exclude_cs <- c(exclude_cs,paste0('FE_z_level__',2:num_z))
  }
  
  # add in any additional fixed effects (not to be included in predict)
  if(exists('non_pred_fes') & !is.null('non_pred_fes')){
    message(sprintf('Adding in the following non predicted FEs: %s',paste(non_pred_fes,collapse=', ')))
    for(npf in non_pred_fes){
      if(!npf %in% names(d)){
        message(sprintf('Not including %s because it is not in the data table',npf))
        non_pred_fes <- non_pred_fes[non_pred_fes != npf]
      } else {
        if(min(d[[npf]]) == max(d[[npf]])){
          message(sprintf('Not including %s because it does not vary in the data',npf))
          non_pred_fes <- non_pred_fes[non_pred_fes != npf]          
        } else{
          X_xp <- cbind(X_xp,d[[npf]])
          colnames(X_xp) <- c(colnames(X_xp)[-ncol(X_xp)],npf)
          exclude_cs <- c(exclude_cs,npf)
        }
      }
    }
  } else {
    non_pred_fes <- c()
  }
  
  # cs_df. imports seegMBG
  cs_df <- getCentreScale(X_xp, exclude = c('int',exclude_cs))
  X_xp  <- centreScale(X_xp, df = cs_df)
  
  # get data range in case we want to clamp for prediction later
  clamper <- data.table(apply(X_xp,2,range))
  
  # sort nugget and RE indicators
  nugget     <- as.numeric(as.logical(nugget))
  country_re <- as.numeric(as.logical(country_re))
  nid_re     <- as.numeric(as.logical(nid_re)) # these do not get used in prediction
  geo_re     <- as.numeric(as.logical(geo_re))
  
  # check there is more than one observed country or nid if REs for those are set
  if(length(unique(d$country)) == 1 & country_re == TRUE){
    message('WARNING: Only found one unique country in this data frame, so turning off country random effects.')
    country_re <- FALSE
  }
  if(length(unique(d$nid)) == 1 & nid_re == TRUE){
    message('WARNING: Only found one unique NID in this data frame, so turning off NID random effects.')
    nid_re <- FALSE
  }
  
  # make a gaul/country/cntry_RE_idx mapping table
  md    <- get_location_code_mapping(shapefile_version = shapefile_version)
  mdsub <- md[ihme_lc_id %in% unique(as.character(d$country)),]
  if(nrow(mdsub) != length(unique(as.character(d$country)))){
    message(sprintf('get_location_code_mapping() COUNTRY NAMES: %s',paste(sort(mdsub$ihme_lc_id),collapse=', ')))
    message(sprintf('IN DATA COUNTRY NAMES: %s',paste(sort(unique(as.character(d$country))),collapse=', ')))
    stop('get_location_code_mapping() and countries in data not of matching lengths')
  }
  cntry_re_map <- data.table(
                    country   = mdsub$ihme_lc_id,
                    gaul_code = mdsub$GAUL_CODE, ## TODO: MIND THIS FOR THE GADMN TRANSITION. DEPENDS LATER ON MERGING TO A SIMPLE RASTER. Change to loc_id
                    re_id     = 0:(nrow(mdsub)-1))
  cntry_re_vec <- cntry_re_map$re_id[match(as.character(d$country),cntry_re_map$country)]
 
  # make an nid_re mapping table NID NID
  nidEff <- unique(select(d, country, nid)) %>% # get unique nid country combos
    group_by(country) %>% # group by country to see...
    mutate(nidCount=n()) %>% # the number of nids per country
    filter(nidCount!=1) %>% # ignore where we only have 1 nid in a country
    ungroup %>% select(nid) # simplify
  if(nrow(nidEff) != 0){
    nidEff <- nidEff %>%
      mutate(re_id=1:n()) %>% # generate a random id number
      right_join(select(d, nid), by="nid") %>% # merge on the og data to index re
      mutate(re_id=ifelse(is.na(re_id), 0, re_id)) # replace nans with zero effect
  }
  else{
    nidEff <- select(d, nid) %>%
      mutate(re_id=0)
  }
  nid_re_vec <- nidEff$re_id
  
  geoEff <- unique(select(d, country, geo_unit)) %>% # get unique geo/country combos
    group_by(country) %>% # group by country to see...
    mutate(geoCount=n()) %>% # the number of geo units per country
    filter(geoCount!=1) %>% # ignore where we only have 1 geo unit in a country
    ungroup %>% select(geo_unit) # simplify
  if(nrow(geoEff) != 0){
    geoEff <- geoEff %>%
      mutate(geo_id=1:n()) %>% # generate a random id number
      right_join(select(d, geo_unit), by="geo_unit") %>% # merge on the og data to index re
      mutate(geo_id=ifelse(is.na(geo_id), 0, geo_id)) # replace nans with zero effect
  }
  else{
    geoEff <- select(d, geo_unit) %>%
      mutate(geo_id=0)
  }
  geo_re_vec <- geoEff$geo_id
  
  # sort country RE, for now just intercept. later add Random slope by zcol
  
  
  # set GP RE array, or matrix depending on if we have a z dimension
  if(num_z > 1) {
    Epsilon_stz <- array(0, dim=c(mesh$n,length(yl),num_z))
  } else {
    Epsilon_stz <- array(0, dim=c(mesh$n,length(yl)))
  }
  
  # if there was a constraint, make sure it is accounted for accross all time age and time periods
  if(gp_sum_to_zero == TRUE){
    A.constr <- spde$f$extraconstr$A
    nnn      <- length(Epsilon_stz)/length(A.constr)
    A.constr <- matrix(rep(A.constr,nnn), nrow=1) 
  } else {
    # if there are no constraints make this zeroes, and it wont constrain the likelihood b/c well multiply by zero
    A.constr <- matrix(0,nrow=1,ncol=length(Epsilon_stz))
  }
  
  # set up vectors of model family 
  # look for convention in the data of lik_fam_<<binom,gauss>>, if not there, default to config family
  lik_gaussian <- lik_binomial <- rep(0, nrow(d))
  if(('lik_fam_binom' %in% names(d)) & ('lik_fam_gauss' %in% names(d))) {
    lik_gaussian <- d$lik_fam_gauss
    lik_binomial <- d$lik_fam_binom
    message(sprintf('Found row specific data likelihood indicators, will use those. %i rows binom, %i rows gauss',
                    sum(lik_binomial),sum(lik_gaussian)))
  } else {
    if(indicator_family == 'binomial') {
      lik_binomial <- rep(1, nrow(d))
      message('Using indicator family binomial for all rows')
    } else if(indicator_family == 'gaussian') {
      lik_gaussian <- rep(1, nrow(d))
      message('Using indicator family gaussian for all rows')
    }
  }

  if(any(lik_gaussian+lik_binomial != 1))
    stop('Not all rows in your data have been assigned a model (binom or gauss), or some have been assigned multiple!')
  
  # also look for sd if already exists in the data for the gauss rows to use
  # This is useful for crosswalked values with some data uncertainty, convention is variable named sd_<<INDICATOR>>
  sd_i <- rep(0, nrow(d))
  if(paste0('sd_',indicator) %in%  names(d)){
    message('Found SD estimates to use for crosswalked values.')
    sd_i <- d[[paste0('sd_',indicator)]]
    sd_i[is.na(sd_i)] <- 0
  }


  # run a quick regression to get starting values for the fixed effects 
  # This can speed up model fitting if iterations are slow.
  # Note this fit is currently assuming binomial
  print(sum(lik_binomial))
  print(sum(lik_gaussian))
  if(all(lik_binomial==1)){
    message('LM for starting fe vals')
    y <- (d[[indic]][lik_binomial==1]+.0001)/d$N[lik_binomial==1]
    y[y<=0] <- 0.001
    y[y>=1] <- 0.999
    fe_start <- round( unname( lm(qlogis(y) ~ -1 + X_xp[lik_binomial==1,])$coefficients ), 4)
  } else {
    message('Default starting fe vals')
    fe_start <- rep(0,ncol(X_xp))
  }
  message(sprintf('starting values for fixed effects: %s',paste0(fe_start,collapse=', ')))
  
  
  # cannot allow a gaussian likelihood to also have a nugget in the linear term, it leads to issues
  if(nugget == 1 & all(lik_gaussian == 1)){
    message('WARNING:: Nugget in all gaussian model leads to identifiability issues. Removing nugget for you.')
    nugget <- 0
  }
  
  
  # check if user wants to scale gaussian variance by N, if not set them all to one in the gaussian rows
  n_i <- d$N
  if(scale_gaussian_variance_N == FALSE & sum(lik_gaussian)>1) {
    message('Not scaling gaussian error by N since scale_gaussian_variance_N == FALSE.')
    n_i[lik_gaussian==1] <- 1
  }
  
  # NOTE (RB 2AUG2018) I have concerns about identifiability in situations where there is one survey in a 
  #  country and country and nid REs are both turned on
  # identify these obervations and pass them to model so that the model can ignore nid random effects in these situation 
  #  another option could be to use the map to set these values to zero in this case. Waiting first to see if this actually causes convergence
  #  issues before dealing with it
  # For now simply drop NID random effect altogether from the model if this is the case. Eventually see if we can use map to set specific REs
  #if(all(country_re == TRUE & nid_re == TRUE)){
  #  if( any(colSums(table(nid_re_vec,cntry_re_vec) != 0) == 1) ){
  #    message('WARNING: possible identifiability issue identified, where NID and CNTRY REs are both on and at least one country has only one survey. Turning off NID random effect as a remedy for now.')
  #    nid_re <- FALSE
  #  }
  #}
  
  # if there is only one country in the region, turn off country_re
  if(all(country_re == TRUE & length(get_gaul_codes(reg)) == 1)){
    message('WARNING: One country in this region, so turning off country random effects')
    country_re <- FALSE
  }
  
  # print some messages for random effectss
  if(nugget == TRUE)     message('USING NUGGET (INDIVIDUAL OBSERVATION) RANDOM EFFECTS')
  if(country_re == TRUE) message('USING COUNTRY RANDOM EFFECTS')
  if(nid_re == TRUE)     message('USING NID RANDOM EFFECTS')
  if(geo_re == TRUE)     message('USING GEOUNIT RANDOM EFFECTS')
  
  # Construct a list of all Data necessary to TMB to fit
  Data <- list(num_i          = nrow(d),               # Total number of observations
               num_s          = mesh$n,                # Number of vertices in SPDE mesh
               num_t          = length(yl),            # Number of periods
               num_z          = num_z,                 # 3rd dimension for GP, 
               y_i            = d[[indic]],            # Number of observed events in the cluster (N+ in binomial likelihood)
               n_i            = d$N,                   # Number of observed exposures in the cluster (N in binomial likelihood)
               t_i            = d$period-1,            # Sample period ( starting at zero because C)
               c_re_i         = cntry_re_vec,          # vector of country ids, ( starting at zero because C)
               nid_re_i       = nid_re_vec,            # vector of survey ids, zero index added in cpp for null effect
               geo_re_i       = geo_re_vec,            # vector of geography ids, zero index added in cpp for null effect
               w_i            = d$weight,              # Data weight for each row
               X_ij           = X_xp,                  # Covariate design matrix
               M0             = spde$param.inla$M0,    # SPDE sparse matrix
               M1             = spde$param.inla$M1,    # SPDE sparse matrix
               M2             = spde$param.inla$M2,    # SPDE sparse matrix
               Aproj          = A.proj,                # mesh to prediction point projection matrix
         #      Aconstraint    = as.vector(A.constr),   # constrain GP to zero matrix (areas of cones)
               lik_gaussian_i = lik_gaussian,          # data likelihood for each row
               lik_binomial_i = lik_binomial,          # data likelihood for each row
               sd_i           = sd_i,                  # crossalked standard deviation
               options = list(
                 use_priors = 1,                   # option1==1 use priors 
                 adreport_off = 1,                   # option2==1 ADREPORT off
                 nugget = nugget,              # option3==1 include nugget
                 country_random = country_re,          # option4==1 country random effects
                 NID_random = nid_re,              # option5==1 NID random effects
                 GEO_random = geo_re,
                 useGP = as.numeric(as.logical(use_gp))),
               prior_log_nugget_sigma = read_inla_prior(nugget_prior),
               prior_log_cre_sigma = read_inla_prior(ctry_re_prior),
               prior_log_nidre_sigma = read_inla_prior(nid_re_prior),
               prior_log_geore_sigma = read_inla_prior(geo_re_prior),
               fconstraints = tmb_cov_constraint(colnames(X_xp), cov_constraints)
               )
  
  # Set staring values for parameters
  Parameters <- list(alpha_j          = fe_start,  # FE parameters alphas
                     logtau           = -0.5,                           # Matern/AR tau
                     logkappa         = -0.5,                        # Matern Range
                     trho             = 0.95,                          # temporal rho
                     zrho             = 0.95,                          # 3rd dimension of GP rho (TODO)
                     log_nugget_sigma = -1,                            # log(SD) of the normal nugget term
                     log_cre_sigma    = -1,                            # log(SD) of the normal country intercept term (later add slopes as vector)
                     log_nidre_sigma  = -1,                            # log(SD) of the normal NID intercept term 
                     log_geore_sigma  = -1,                            # log(SD) of the normal geo intercept term 
                     log_gauss_sigma  = -1,                            # log(SD) of normal model
                     Epsilon_stz      = Epsilon_stz,                   # Random Effects: GP locations
                     nug_i            = rep(0,nrow(d)),                # Random Effects: Nugget Values
                     cntry_re         = rep(0,nrow(cntry_re_map)),     # Random Effects Values of country random effects (later add in slope stuff)
                     nid_re           = rep(0,max(nidEff$re_id)),      # Random Effects Values of nid random effects (later add in slope stuff)
                     geo_re           = rep(0,max(geoEff$geo_id)))
  
  
  # put bounds on parameters (Note, this is especially important for rhos)
  L  <- c(rep(-10,ncol(X_xp)),-10,-10,-99,-99,-10,-10,-10) ## updated the rho limits from .99999 since I transformed them in the cpp 
  U  <- c(rep( 10,ncol(X_xp)), 10, 10, 99, 99, 10, 10, 10)
  pn <- c(rep('alpha_j',ncol(X_xp)),'logtau','logkappa','trho','zrho','log_nugget_sigma','log_cre_sigma','log_nidre_sigma')
  names(L) <- names(U) <- pn
  
  # return the list
  return(list(Data         = Data,
              Parameters   = Parameters,
              cs_df        = cs_df,
              clamper      = clamper,
              coords       = coords,
              mesh         = mesh,
              cntry_re_map = cntry_re_map,
              non_pred_fes = non_pred_fes,
              L            = L,
              U            = U))

}
