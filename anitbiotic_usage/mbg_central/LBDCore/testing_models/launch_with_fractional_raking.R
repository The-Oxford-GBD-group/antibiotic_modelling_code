###############################################################################
###############################################################################
## Basic Launch script for Nov 2017 MBG intensive. RB
## (Edited for testing with LBDCore)
## Launch with: 
"
qsub -N training_launch_fraxrake -l m_mem_free=10G -l fthread=3 \
-l h_rt=05:00:00 -q geospatial.q -P proj_geo_nodes \
-v sing_image=/share/singularity-images/lbd/testing_INLA_builds/lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v3.simg \
-p 0 -j y -o /share/temp/sgeoutput/$USER/output \
/share/code/geospatial/sadatnfs/lbd_core/mbg_central/share_scripts/shell_sing.sh \
/share/code/geospatial/sadatnfs/lbd_core/mbg_central/LBDCore/testing_models/launch_with_fractional_raking.R
"
###############################################################################
###############################################################################


###############################################################################
## SETUP
###############################################################################

## clear environment
rm(list = ls())

## Set core_repo location and indicator group
user <- Sys.info()["user"]
core_repo <- paste0("/share/code/geospatial/", user, "/lbd_core")
indicator_group <- "training"
indicator <- "tr_had_diarrhea"
sharedir <- paste("/share/geospatial/mbg", indicator_group, indicator, sep = "/")
commondir <- paste(core_repo, "mbg_central/share_scripts/common_inputs", sep = "/")


# Load MBG packages and functions
message("Loading in required R packages and MBG functions")
library(LBDCore, lib.loc = sprintf("~/R/x86_64-pc-linux-gnu-library/%s.%sgeo/", R.Version()$major, R.Version()$minor))

## Create run date in correct format
run_date <- "sadatnfs_frax_rake_2019_06_19_10_25_16"
# run_date <- paste0(user, "_frax_rake_", make_time_stamp(TRUE))

## Set up configs
config_list <- set_up_config(
  repo = core_repo,
  core_repo = core_repo,
  indicator_group = indicator_group,
  indicator = indicator,
  config_name = "config_training_frax",
  covs_name = "covs_training"
)

## Separate out config and FE ones into global environment
list2env(config, envir = .GlobalEnv)


## Create a few objects from the config file loaded above
if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text = Regions))
if (class(year_list) == "character") year_list <- eval(parse(text = year_list))
if (length(summstats) == 1 & grepl(",", summstats)) summstats <- eval(parse(text = summstats))

## Load gaul list
gaul_list <- get_adm0_codes(Regions, shapefile_version = modeling_shapefile_version)

## If running individual countries, get set up
if (individual_countries == TRUE) {
  # Convert all Regions to individual countries
  Regions <- get_individual_countries(gaul_list)

  # Turn off all FEs
  use_child_country_fes <- FALSE
  use_inla_country_fes <- FALSE
  use_inla_country_res <- FALSE
}

###############################################################################
## Make Holdouts
###############################################################################
if (makeholdouts) {
  # load the full input data
  df <- load_input_data(
    indicator = indicator,
    simple = NULL,
    removeyemen = TRUE,
    years = yearload,
    yl = year_list,
    withtag = as.logical(withtag),
    datatag = datatag,
    use_share = as.logical(use_share)
  )

  # add in location information
  df <- merge_with_ihme_loc(df, shapefile_version = modeling_shapefile_version)

  # make a list of dfs for each region, with 5 qt folds identified in each
  stratum_ho <- make_folds(
    data = df,
    n_folds = as.numeric(n_ho_folds),
    spat_strat = "qt",
    temp_strat = "prop",
    strat_cols = "region",
    ts = as.numeric(ho_ts),
    mb = as.numeric(ho_mb)
  )
}


###############################################################################
## Launch Parallel Script
###############################################################################

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if (makeholdouts) loopvars <- expand.grid(Regions, 0, 0:n_ho_folds) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for (i in 1:nrow(loopvars)) {
  message(paste(loopvars[i, 2], as.character(loopvars[i, 1]), loopvars[i, 3]))

  # make a qsub string
  qsub <- make_qsub_share(
    age = loopvars[i, 2],
    reg = as.character(loopvars[i, 1]),
    code = "../mbg_central/share_scripts/parallel_model_pkgtest",
    holdout = loopvars[i, 3],
    test = TRUE,
    indic = indicator,
    saveimage = TRUE,
    addl_job_name = eval(parse(text = jn)), ## from config
    memory = 5.5,
    cores = 1,
    singularity = "/share/singularity-images/lbd/testing_INLA_builds/lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v3.simg",
    queue = "geospatial.q",
    run_time = "1:00:00",
    proj = "proj_geo_nodes"
  )

  system(qsub)
}


## check to make sure models are done before continuing
waitformodelstofinish(lv = cbind(as.character(loopvars[, 1]), loopvars[, 3]), sleeptime = 60)

##############################################################################
## Summarize model results
##############################################################################

clean_model_results_table()

# Stop if individual countries (no need for post-est)
if (as.logical(individual_countries) == F) {

  ###############################################################################
  ## Post-Estimation and aggregation !!!!
  ###############################################################################

  # ## Save strata for Shiny to use in producing aggregated fit statistics
  strata <- unique(as.character(loopvars[, 1]))
  # dir.create(paste0(sharedir, "/fit_stats"), showWarnings = FALSE)
  # save(strata, file = paste0(sharedir, "/fit_stats/strata.RData"))

  ## Load GBD Estimates for this indicator which will be used in raking
  gbd <- get_gbd_estimates(
    gbd_name = 302,
    region = "africa",
    measure_id = 5,
    age_group_id = 1,
    metric_id = 3,
    year_ids = year_list,
    shapefile_version = raking_shapefile_version,
    rake_subnational = subnational_raking,
    gbd_round_id = 4
  )
  

  ## Prepare for parallel post-estimation - save file with objs to re-load in child processes
  prep_postest(
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    save_objs = c(
      "core_repo", "gbd", "year_list", "summstats",
      "rake_transform", "pop_measure", "config"
    )
  )

  ## Parallelized post-estimation over region
  #### THIS ALSO AGGREGATES ####
  postest_script <- "postest_frax_script"

  for (s in strata) {
    qsub <- make_qsub_postest(
      code = postest_script,
      stratum = s,
      log_location = "sharedir",
      memory = 9.,
      subnat_raking = subnational_raking,
      modeling_shapefile_version = modeling_shapefile_version,
      raking_shapefile_version = raking_shapefile_version,
      singularity = "/share/singularity-images/lbd/testing_INLA_builds/lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v3.simg",
      queue = "geospatial.q",
      run_time = "1:00:00",
      proj = "proj_geo_nodes",
      cores = 2
    )
    system(qsub)
  }

  ## check to make sure post-est done before continuing
  waitformodelstofinish(lv = cbind(strata, 0), sleeptime = 60)

  ## Combine post est stuff across regions and save needed outputs
  post_load_combine_save(summstats = summstats)

  # Clean up / delete unnecessary files
  clean_after_postest(
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    strata = strata,
    delete_region_rasters = F
  )

  ###############################################################################
  ## Launch model diagnostics script for shiny tool
  ###############################################################################
  # View results at https://shiny.ihme.washington.edu/connect/#/apps/119/

  make_model_diagnostics(
    indic = indicator,
    ig = indicator_group,
    rd = run_date,
    cores = length(Regions),
    memory = 3.5,
    queue = 'geospatial.q',
    proj = 'proj_geo_nodes',
    run_time = '01:00:00'
  )

  ###############################################################################
  ## Aggregate to admin2, admin1, and national levels
  ###############################################################################

  combine_aggregation(
    rd = run_date, indic = indicator, ig = indicator_group,
    ages = 0,
    regions = strata,
    holdouts = 0,
    raked = c(T, F)
  )

  summarize_admins(
    summstats = c("mean", "upper", "lower", "cirange"),
    ad_levels = c(0, 1, 2),
    raked = c(T, F)
  )
  message("After summarize_admins - about to save CSV")

  # Combine csv files
  csvs <- list.files(paste0(sharedir, "/output/", run_date, "/"),
    pattern = "input_data(.*).csv",
    full.names = T
  )

  csv_master <- lapply(csvs, fread) %>%
    rbindlist() %>%
    subset(., select = names(.) != "V1")
  write.csv(csv_master, file = paste0(sharedir, "/output/", run_date, "/input_data.csv"))

  ###############################################################################
  ## Create AROC objects & do projections
  ###############################################################################

  # note! you need to use raking_shapefile_version is using raked results
  make_aroc(
    ind_gp = indicator_group,
    ind = indicator,
    rd = run_date,
    matrix_pred_name = NULL,
    type = c("cell", "admin"),
    measure = "prevalence",
    year_list = c(2000:2015),
    uselogit = FALSE,
    raked = FALSE,
    weighting_res = "domain",
    weighting_type = "exponential",
    pow = 1,
    input_data = read.csv(sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/input_data.csv",
      indicator_group, indicator, run_date
    )),
    mult_emp_exp = FALSE,
    extra_file_tag = "_exp_domain",
    shapefile_version = modeling_shapefile_version
  )

  # note! you need to use raking_shapefile_version is using raked results
  make_proj(
    ind_gp = indicator_group,
    ind = indicator,
    rd = run_date,
    type = c("cell", "admin"),
    proj_years = c(2020, 2025, 2030),
    measure = "prevalence",
    skip_cols = NULL,
    year_list = c(2000:2015),
    uselogit = FALSE,
    raked = FALSE,
    extra_file_tag = "_exp_domain",
    shapefile_version = modeling_shapefile_version
  )

  
  
  ###############################################################################
  # Rake to FHS estimates with a new postest script
  ###############################################################################
  
  ## For this exercise, we will create a dummy forecast from the GBD output to rake to
  message("Using a simple loess to create forecasts to the 'gbd' object.")
  message("Please provide your own gbd_forecast object if you have acquired it apriori.")
  message("The columns must have : name (GBD location_id), year (year_id) and mean (the mean estimates).")
  gbd_forecast <- loess_extrap_gbd_input_by_loc(gbd = gbd, year_list = year_list, year_forecast_end = 2030)
  
  ## Create a folder path to save the new projections and outputs to:
  fhs_proj_folder <- "FHS_proj"
  fhs_raked_outputs <- paste0(sharedir, "/output/", run_date, "/", fhs_proj_folder, "/")
  dir.create(fhs_raked_outputs)
  
  ## Set up years of projection
  proj_years <- c(2020, 2025, 2030)
  
  ## Set measure
  measure <- "prevalence"
  
  
  ## Prepare for parallel post-estimation - save file with objs to re-load in child processes
  prep_postest(
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    save_objs = c(
      "core_repo", "gbd_forecast", "summstats",
      "rake_transform", "pop_measure", "config", 
      "fhs_raked_outputs", "proj_years", "measure"
    )
  )
  
  ## Parallelized post-estimation over region
  #### THIS ALSO AGGREGATES ####
  postest_script <- "postest_frax_future_script"
  
  for (s in strata) {
    qsub <- make_qsub_postest(
      code = postest_script,
      stratum = s,
      log_location = "sharedir",
      memory = 5,
      subnat_raking = subnational_raking,
      modeling_shapefile_version = modeling_shapefile_version,
      raking_shapefile_version = raking_shapefile_version,
      singularity = "/share/singularity-images/lbd/testing_INLA_builds/lbd_rpkgs3.6.0gcc9mklrstudioserver1.2.1511_v3.simg",
      queue = "geospatial.q",
      run_time = "1:00:00",
      proj = "proj_geo_nodes",
      cores = 2,
      addl_job_name = "_PROJ"
    )
    system(qsub)
  }
  
  ## check to make sure post-est done before continuing
  waitformodelstofinish(lv = cbind(strata, 0), sleeptime = 60)
  
  ## Combine post est stuff across regions and save needed outputs
  post_load_combine_save(summstats = summstats,
                         proj = TRUE,
                         proj_folder = fhs_proj_folder,
                         run_summ = FALSE)
  
  # Clean up / delete unnecessary files
  clean_after_postest(
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    strata = strata,
    delete_region_rasters = F
  )
  
  
  
  
  
  ###############################################################################
  # Look at performance against goals
  ###############################################################################

  # Define goals: start by initializing goal object
  goals <- add_goal(
    target_year = 2030,
    target = 0.03,
    target_type = "less",
    abs_rel = "absolute",
    pred_type = c("cell", "admin")
  )

  # Add goals to existing goal object by specifying goal_obj
  goals <- add_goal(
    goal_obj = goals,
    target_year = 2020,
    target = 0.04,
    target_type = "less",
    abs_rel = "absolute",
    pred_type = c("cell", "admin")
  )

  # Run comparisons
  compare_to_target(
    ind_gp = indicator_group,
    ind = indicator,
    rd = run_date,
    goal_obj = goals,
    measure = "prevalence",
    year_list = c(2000:2015),
    uselogit = FALSE,
    shapefile_version = modeling_shapefile_version
  )

  ###############################################################################
  # Make summary metrics
  ###############################################################################

  # Get in and out of sample draws
  run_in_oos <- get_is_oos_draws(
    ind_gp = indicator_group,
    ind = indicator,
    rd = run_date,
    ind_fm = "binomial",
    model_domain = "africa",
    age = 0,
    nperiod = length(year_list),
    yrs = year_list,
    get.oos = as.logical(makeholdouts),
    write.to.file = TRUE,
    shapefile_version = modeling_shapefile_version
  )

  ## set out_dir
  out_dir <- paste0(sharedir, "/output/", run_date, "/summary_metrics/")
  dir.create(out_dir, recursive = T, showWarnings = F)

  ## for admin0
  draws.df <- fread(sprintf(
    "/share/geospatial/mbg/%s/%s/output/%s/output_draws_data.csv",
    indicator_group, indicator, run_date
  ))

  country.pvtable <- get_pv_table(
    d = draws.df,
    indicator_group = indicator_group,
    rd = run_date,
    indicator = indicator,
    aggregate_on = "country",
    draws = as.numeric(samples),
    out.dir = out_dir
  )

  write.csv(country.pvtable,
    file = sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/country_metrics.csv",
      indicator_group, indicator, run_date
    )
  )

  ad1.pvtable <- get_pv_table(
    d = draws.df,
    indicator_group = indicator_group,
    rd = run_date,
    indicator = indicator,
    aggregate_on = "ad1",
    draws = as.numeric(samples),
    out.dir = out_dir
  )
  write.csv(ad1.pvtable,
    file = sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/ad1_metrics.csv",
      indicator_group, indicator, run_date
    )
  )
  ad2.pvtable <- get_pv_table(
    d = draws.df,
    indicator_group = indicator_group,
    rd = run_date,
    indicator = indicator,
    aggregate_on = "ad2",
    draws = as.numeric(samples),
    out.dir = out_dir
  )
  write.csv(ad2.pvtable,
    file = sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/summary_metrics/ad2_metrics.csv",
      indicator_group, indicator, run_date
    )
  )

  ###############################################################################
  ## Launch Validation Report (deprecated)
  ###############################################################################

  # dir.create(paste0(sharedir, '/output/', run_date, '/val_logs/'))

  # ## Launch validation reports
  # lapply(strata, submit_validation_report,
  #                    indicator       = indicator,
  #                    indicator_group = indicator_group,
  #                    run_date        = run_date,
  #                    pop_measure     = pop_measure,
  #                    repo            = repo,
  #                    log_dir         = paste0(sharedir, '/output/', run_date, '/val_logs/'),
  #                    target_type     = target_type,
  #                    target
  #                            = as.numeric(st_targ),
  #                    geo_nodes       = use_geo_nodes)
} # Close loop for individual countries

## END OF FILE
###############################################################################
