###############################################################################
## MBG launch script
##
## Example launch script for MBG training
##
###############################################################################


## Setup -------------------------------------------------------------------------

## clear environment
rm(list=ls())

## LOAD LBD LIBRARY
personal_lib <- ifelse(grepl('health_fin', Sys.getenv("SINGULARITY_NAME")) ,  
                       "~/R/x86_64-pc-linux-gnu-library/3.5/", 
                       "~/R/x86_64-pc-linux-gnu-library/3.5geo/")
if(!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)
Sys.setenv(R_LIBS_USER = personal_lib)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()) )
# devtools::install("/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore", dependencies=F, upgrade=F)

## Set repo location, indicator group, and some arguments
user            <- Sys.info()['user']
indicator_group <- 'training'
indicator       <- 'ors'
core_repo       <- paste0('/share/code/geospatial/', user, '/lbd_core/')
indicator_repo  <- paste0('/share/code/geospatial/', user, '/', indicator_group, '/')
config_par      <- 'config_training'
cov_par         <- 'covs_training' 
Regions         <- 'PER'
message(indicator)

## Load LBDCore
library(LBDCore)

## Read config file and save all parameters in memory
config <- load_config(repo            = indicator_repo,
                      indicator_group = '',
                      indicator       = '',
                      config_name     = config_par,
                      covs_name       = cov_par)

## Ensure you have defined all necessary settings in your config
check_config(cr = core_repo)



## Set project
proj <- ifelse(as.logical(use_geos_nodes), 'proj_geo_nodes', 'proj_geospatial')

## Create run date (really a comment) to store model outputs in
run_date <- paste0('training_', user, '_packageTest')

## Create output folder with the run_date
outputdir      <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/')
dir.create(outputdir)

## Make sure year object is in the correct format
if (class(year_list) == 'character') year_list <- eval(parse(text=year_list))

## If running individual countries make sure all country FEs and REs off
if (nchar(Regions[1]) == 3) individual_countries <- TRUE
if (individual_countries) {
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}


## Make holdouts -------------------------------------------------------------------------

if(makeholdouts){
  message('Making holdouts')
  
  # load the full input data
  df <- load_input_data(indicator   = indicator,
                        simple      = NULL,
                        removeyemen = TRUE,
                        years       = yearload,
                        withtag     = as.logical(withtag),
                        datatag     = datatag,
                        use_share   = as.logical(use_share))
  
  # add in location information
  df <- merge_with_ihme_loc(df)
  
  # make a list of dfs for each region, with 5 qt folds identified in each
  stratum_ho <- make_folds(data       = df,
                           n_folds    = as.numeric(n_ho_folds),
                           spat_strat = 'qt',
                           temp_strat = 'prop',
                           strat_cols = 'region',
                           ts         = as.numeric(ho_ts),
                           mb         = as.numeric(ho_mb))
}


## Launch parallel script -------------------------------------------------------------------------

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if(makeholdouts) loopvars <- expand.grid(Regions, 0, 0:n_ho_folds) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for(i in 1:nrow(loopvars)){
  
  message(paste(loopvars[i,2],as.character(loopvars[i,1]),loopvars[i,3]))
  
  # make a qsub string
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = F,
                          indic         = indicator,
                          saveimage     = TRUE,
                          memory        = ifelse(individual_countries, 10, 50),
                          cores         = ifelse(individual_countries, 5, 10),
                          proj          = proj,
                          geo_nodes     = as.logical(use_geos_nodes),
                          corerepo      = core_repo,
                          code          = NULL,
                          addl_job_name = paste0('training_', user),
                          singularity   = 'default')
  # submit job
  print(qsub)
  
}

