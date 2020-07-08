# This is the file used to create several of the config rda files required
# by the LBDCore package

# Must haves are included here as a character
must_haves <- data.table::data.table(V1 = c(
  "check_cov_pixelcount",
  "coefs_sum1",
  "constrain_children_0_inf",
  "countries_not_to_rake",
  "countries_not_to_subnat_rake",
  "ctry_re_prior",
  "datatag",
  "fit_with_tmb",
  "fixed_effects",
  "fixed_effects_measures",
  "gbd_fixed_effects",
  "gbd_fixed_effects_age",
  "gbd_fixed_effects_measures",
  "gbm_bf",
  "gbm_lr",
  "gbm_tc",
  "ho_mb",
  "ho_ts",
  "holdout_strategy",
  "indicator_family",
  "individual_countries",
  "inla_cores",
  "intercept_prior",
  "interval_mo",
  "intstrat",
  "jn",
  "keep_inla_files",
  "lat_col",
  "long_col",
  "makeholdouts",
  "memory",
  "mesh_s_max_edge",
  "mesh_s_offset",
  "mesh_t_knots",
  "metric_space",
  "modeling_shapefile_version",
  "n_ho_folds",
  "n_stack_folds",
  "nugget_prior",
  "other_weight",
  "pop_measure",
  "pop_release",
  "queue",
  "rake_countries",
  "rake_transform",
  "raking_shapefile_version",
  "rho_prior",
  "run_time",
  "s2_mesh_params",
  "samples",
  "scale_gaussian_variance_N",
  "singularity_version",
  "skip.inla",
  "skip.stacking",
  "skipinla",
  "skiptoinla",
  "skiptoinla_from_rundate",
  "slots",
  "spat_strat",
  "ss_col",
  "st_targ",
  "stacked_fixed_effects",
  "stackers_in_transform_space",
  "subnat_country_to_get",
  "subnational_raking",
  "summstats",
  "target_type",
  "temp_strat",
  "test",
  "test_pct",
  "time_stamp",
  "transform",
  "use_child_country_fes",
  "use_geos_nodes",
  "use_gp",
  "use_inla_country_fes",
  "use_inla_country_res",
  "use_inla_nugget",
  "use_nid_res",
  "use_raw_covs",
  "use_s2_mesh",
  "use_share",
  "use_stacking_covs",
  "use_subnat_res",
  "withtag",
  "year_list",
  "yearload",
  "yr_col",
  "z_list",
  "zcol"
))

# config values add another column to the data.table with the default values
config_values <- data.table::as.data.table(list(
  check_cov_pixelcount = FALSE,
  coefs_sum1 = TRUE,
  constrain_children_0_inf = FALSE,
  countries_not_to_rake = "ESH+GUF",
  countries_not_to_subnat_rake = "PHL+NGA+PAK+ETH+KEN",
  ctry_re_prior = "list(prior = 'loggamma', param = c(1, 5e-5))",
  datatag = NA,
  fit_with_tmb = FALSE,
  fixed_effects = NA,
  fixed_effects_measures = "mean + total",
  gbd_fixed_effects = NA,
  gbd_fixed_effects_age = "c(2, 3, 4, 5)",
  gbd_fixed_effects_measures = NA,
  gbm_bf = 0.75,
  gbm_lr = 0.005,
  gbm_tc = 3,
  ho_mb = 10,
  ho_ts = 500,
  holdout_strategy = "nids",
  indicator_family = "binomial",
  individual_countries = FALSE,
  inla_cores = 5,
  intercept_prior = 0,
  interval_mo = 12,
  intstrat = "eb",
  jn = "sprintf('')",
  keep_inla_files = FALSE,
  lat_col = "latitude",
  long_col = "longitude",
  makeholdouts = FALSE,
  memory = 10,
  mesh_s_max_edge = "c(0.2, 5)",
  mesh_s_offset = "c(1, 5)",
  mesh_t_knots = "c(1, 6, 11, 16)",
  modeling_shapefile_version = "current",
  n_ho_folds = 5,
  n_stack_folds = 5,
  nugget_prior = "list(prior = 'loggamma', param = c(1, 5e-5))",
  other_weight = NA,
  pop_measure = "a0004t",
  pop_release = "get_newest_worldpop_release(config)",
  queue = "long.q",
  rake_countries = TRUE,
  rake_transform = "none",
  raking_shapefile_version = "current",
  rho_prior = "list(prior = 'normal', param = c(0, 1/(2.58^2)))",
  run_time = "16:00:00:00",
  s2_mesh_params = "c(25, 500, 1000)",
  samples = 250,
  scale_gaussian_variance_N = TRUE,
  singularity_version = "default",
  skip.inla = 0,
  skip.stacking = 0,
  skipinla = FALSE,
  skiptoinla = FALSE,
  skiptoinla_from_rundate = "NULL",
  slots = 10,
  solve = TRUE,
  spat_strat = "qt",
  spde_prior = "list(type='pc')",
  ss_col = "weighted_n",
  st_targ = 0.05,
  stacked_fixed_effects = "gam + lasso + gbm",
  stackers_in_transform_space = TRUE,
  subnat_country_to_get = NA,
  subnational_raking = TRUE,
  summstats = "c('mean', 'cirange', 'upper', 'lower')",
  target_type = "less",
  temp_strat = "prop",
  test = FALSE,
  test_pct = 5,
  time_stamp = TRUE,
  transform = "inverse-logit",
  use_child_country_fes = FALSE,
  use_geos_nodes = TRUE,
  use_gp = TRUE,
  use_inla_country_fes = FALSE,
  use_inla_country_res = TRUE,
  use_inla_nugget = TRUE,
  use_nid_res = FALSE,
  use_raw_covs = FALSE,
  use_s2_mesh = FALSE,
  use_share = TRUE,
  use_stacking_covs = TRUE,
  use_subnat_res = FALSE,
  withtag = FALSE,
  year_list = "c(2000:2017)",
  yearload = "annual",
  yr_col = "year",
  z_list = 0,
  zcol = "z_column_default_blank",
  metric_space = "rates"
))

# for the description add a new vector to the vector of vectors and make sure
# that there is three entries. a variable entry, a default_value entry, and a
# description entry
config_description <- data.table::as.data.table(matrix(c(
  c(
    variable = "check_cov_pixelcount",
    default_value = "TRUE",
    description = "Logical: Should covariates with more NA pixels be automatically dropped to avoid NAs coming through in prediction? Threshold for dropping is 95% of max non-NA pixels a covariate has within the simplified modeling boundary."
  ),
  c(
    variable = "coefs_sum1",
    default_value = "TRUE",
    description = "Logical: Should the coefficients on the stackers sum-to-1? Should probably be TRUE if running a model with stackers. For other models (e.g. GP only or raw/non-stacked covariates) should be FALSE"
  ),
  c(
    variable = "constrain_children_0_inf",
    default_value = "FALSE",
    description = "Logical: Force the coefficient on the stackers to be positive? EXPERIMENTAL! Does not work with coefs_sum1==TRUE"
  ),
  c(
    variable = "countries_not_to_rake",
    default_value = "ESH+GUF",
    description = "String: Countries to not rake at all. Default: French Guiana and Western Sahara"
  ),
  c(
    variable = "countries_not_to_subnat_rake",
    default_value = "PHL+NGA+PAK+ETH+KEN",
    description = "String: Countries to not do subnational raking to GBD. Default: PHL, NGA, PAK, IND, KEN"
  ),
  c(
    variable = "ctry_re_prior",
    default_value = "list(prior = 'loggamma', param = c(1, 5e-5))",
    description = "String of R list notation containing an INLA style prior. Note that this prior is actually placed on on the precision for the error. NOTE: for somewhat historical reasons, the default is set to: \"list(prior = 'loggamma', param = c(2, 1))\". which is probably NOT a good prior. Please investigate and set your own."
  ),
  c(
    variable = "datatag",
    default_value = "",
    description = "String: Name of tag of custom dataset if using withtag='TRUE'. i.e. regular_data_set_name_<datatag>.csv. Can otherwise be left blank."
  ),
  c(
    variable = "fit_with_tmb",
    default_value = "FALSE",
    description = "Logical: Fit model with TMB instead of INLA?"
  ),
  c(
    variable = "fixed_effects",
    default_value = "",
    description = "String: in particular format of 'cov1 + cov2 + ... + cov_n' Note the spaces around the plus signs! This specifies which geospatial fixed effects to load if you're not using the separate cov config."
  ),
  c(
    variable = "fixed_effects_measures",
    default_value = "mean + total",
    description = "String: in particular format of 'meas1 + meas2 + ... + meas_n' Note the spaces around the plus signs! This specifies the measure associated with fixed_effects if cov_config is not used. Measures can be mean, median, total ..."
  ),
  c(
    variable = "gbd_fixed_effects",
    default_value = "",
    description = "String: analogous to fixed_effects"
  ),
  c(
    variable = "gbd_fixed_effects_age",
    default_value = "c(2, 3, 4, 5)",
    description = "String of R vector notation containing integers: Collapse GBD age ranges."
  ),
  c(
    variable = "gbd_fixed_effects_measures",
    default_value = "",
    description = "String: analogous to fixed_effects_measures listed above"
  ),
  c(
    variable = "gbm_bf",
    default_value = "0.75",
    description = "Real between 0 and 1: Bagging fraction in BRT fit"
  ),
  c(
    variable = "gbm_lr",
    default_value = "5.00E-03",
    description = "Real between 0 and 1: Learning rate in BRT fit"
  ),
  c(
    variable = "gbm_tc",
    default_value = "3",
    description = "Integer > 0: Tree complexity in BRT fit"
  ),
  c(
    variable = "ho_mb",
    default_value = "10",
    description = "Integer: Used in quad-tree holdouts. Is the minimum number of points allowed in any one rectangle (min in box)"
  ),
  c(
    variable = "ho_ts",
    default_value = "500",
    description = "Integer: Used in quad-tree holdouts. Is the target total sample size within each quadtree rectangle"
  ),
  c(
    variable = "holdout_strategy",
    default_value = "nids",
    description = "String: TO BE FILLED IN"
  ),
  c(
    variable = "indicator_family",
    default_value = "binomial",
    description = "String: Data likelihood family. Only binomial and gaussian are currently available. More could be added by users if the need arises"
  ),
  c(
    variable = "individual_countries",
    default_value = "FALSE",
    description = "Logical: Should every country in each region by run by itself? Usually set to false unless you want country-specific models for some reason (e.g. using model outputs from individual country fits to generate regions with similar fitted model properties)."
  ),
  c(
    variable = "inla_cores",
    default_value = "5",
    description = "Integer: Number of cores available to INLA inside the parallel script. NOTE: Not currently used!!"
  ),
  c(
    variable = "intercept_prior",
    default_value = "0",
    description = "Real: Prior mean of intercept in INLA fit"
  ),
  c(
    variable = "interval_mo",
    default_value = "12",
    description = "Integer: Number of months between 'years'. Leave at 12 for annual 'year_load', 60 for 5-year bins"
  ),
  c(
    variable = "intstrat",
    default_value = "eb",
    description = "String: Integration strategy of hyperparmeters in INLA. 'eb'=Empirical Bayes is the fastest by far, but only uses modal values of hyperparams and doesn't integrate over them. 'ccd' performs a smart numerical grid integration strategy but takes much longer then 'eb' and the time scales poorly with the number of hyperparameters in model."
  ),
  c(
    variable = "jn",
    default_value = "sprintf('')",
    description = "String. Additional string to be appended to the job names."
  ),
  c(
    variable = "keep_inla_files",
    default_value = "FALSE",
    description = "Logical: Do you want to keep the intermediate INLA files? Strongly suggested to set to FALSE."
  ),
  c(
    variable = "lat_col",
    default_value = "latitude",
    description = "String: Name of column to use in making space(-time) holdouts using methods that use lat-lon"
  ),
  c(
    variable = "long_col",
    default_value = "longitude",
    description = "String: Name of column to use in making space(-time) holdouts using methods that use lat-lon"
  ),
  c(
    variable = "makeholdouts",
    default_value = "FALSE",
    description = "Logical: Run with holdouts?"
  ),
  c(
    variable = "memory",
    default_value = "10",
    description = "Integer: Memory to be allocated in GB"
  ),
  c(
    variable = "mesh_s_max_edge",
    default_value = "c(0.2, 5)",
    description = "String of R vector notation containing two positive reals: First number represents max triangle edge length in mesh inside modeling domain. Second number represents max triangle edge length in mesh outside modeling domain. Increase the first number for testing purposes (for comp speedup but it comes with increased approximation error and mesh artifacts in final predictions). Units are in lat-long degrees."
  ),
  c(
    variable = "mesh_s_offset",
    default_value = "c(1, 5)",
    description = "String of R vector notation containing two reals: Provides an inner and an optional outer extension distance. If negative it is interpreted as a factor relative to the approximate data diameter. If positive it is the extension distance on same scale unit to the coordinates provided. Units are in lat-long degrees."
  ),
  c(
    variable = "mesh_t_knots",
    default_value = "c(1, 6, 11, 16)",
    description = "String of R vector notation containing integer indicies of year_list to place time mesh points. For example, the default \"year_list\" is c(2000:2015), or the 16 consecutive years 2000-2015, inclusive. The default of c(1, 6, 11, 16) for mesh_t_knots would place a time mesh point at year 2000, 2005, 2010, and 2015. At a minimum, place mesh points in the first and last years."
  ),
  c(
    variable = "metric_space",
    default_value = "rates",
    description = "String. Measure of your variable; must be one of 'rates' or 'counts'. Default: 'rates'"
  ),
  c(
    variable = "modeling_shapefile_version",
    default_value = "'current'",
    description = "String: specifies the 'YYYY_MM_DD' version date of a global shapefile set to use in modeling."
  ),
  c(
    variable = "n_ho_folds",
    default_value = "5",
    description = "Integer: If makeholdouts=='TRUE', how many holdouts would you like to make. Usually 5 is selected. Can be any integer greater than 1 (though mostly untested)."
  ),
  c(
    variable = "n_stack_folds",
    default_value = "5",
    description = "Integer: Number of folds to use in stacking cross-validation"
  ),
  c(
    variable = "nugget_prior",
    default_value = "list(prior = 'loggamma', param = c(1, 5e-5))",
    description = "String of R list notation containing an INLA style prior. Note that this prior is actually placed on on the precision for the error. NOTE: for somewhat historical reasons, the default is set to: \"list(prior = 'loggamma', param = c(2, 1))\". which is probably NOT a good prior. Please investigate and set your own."
  ),
  c(
    variable = "other_weight",
    default_value = "",
    description = "String: Name of second weight column. Will be multiplied against the usual weight column to create a combination weighting using both weight columns. Leave blank if you don't plan to use."
  ),
  c(
    variable = "pop_measure",
    default_value = "a0004t",
    description = "String: Population measure to use from the world pop rasters for pop-weighted aggregation type calculations"
  ),
  c(
    variable = "queue",
    default_value = "long.q",
    description = "String. The queue to be used for the new cluster. Default: long.q"
  ),
  c(
    variable = "rake_countries",
    default_value = "TRUE",
    description = "Logical: Should we rake countries at all? Default: TRUE"
  ),
  c(
    variable = "rake_transform",
    default_value = "none",
    description = "String: Could also be 'logit' if you want to perform logit raking"
  ),
  c(
    variable = "raking_shapefile_version",
    default_value = "'current'",
    description = "String: specifies the 'YYYY_MM_DD' version date of a global shapefile set to use in raking and then in post-estimation."
  ),
  c(
    variable = "rho_prior",
    default_value = "list(prior = 'normal', param = c(0, 1/(2.58^2)))",
    description = "String of R list notation containing an INLA style prior. Note that this prior is actually placed on on the transformed parameter: theta = log((1+rho)/(1-rho))"
  ),
  c(
    variable = "run_time",
    default_value = "16:00:00:00",
    description = "String. The run_time to be used for the new cluster. Default: 16 days"
  ),
  c(
    variable = "s2_mesh_params",
    default_value = "c(25, 500, 1000)",
    description = "String of R vector notation containing three positive reals: The first number defines the minimum triangle edge length allowed, the second argument defines how far the mesh should extend past the boundary, and the third argument defines the maximum allowed triangle edge length. All units are in easy to grok kilometers!"
  ),
  c(
    variable = "samples",
    default_value = "250",
    description = "Integer. Number of posterior samples (i.e. candidate maps) to draw from the INLA fit. Usually in the range 250 - 1000."
  ),
  c(
    variable = "scale_gaussian_variance_N",
    default_value = "TRUE",
    description = "Logical: If TRUE and if using a Gaussian likelihood, should the observational variance be scaled by the sample size of the observation? CURRENTLY ONLY AVAILABLE IN TMB"
  ),
  c(
    variable = "singularity_version",
    default_value = "default",
    description = "String: Singularity image version to be used in jobs. Defaults to 'default', which is the latest image detected in '/share/singularity-images/lbd'"
  ),
  c(
    variable = "skip.inla",
    default_value = "0",
    description = "DEPRECATED; use skipinla"
  ),
  c(
    variable = "skip.stacking",
    default_value = "0",
    description = "DEPRECATED; use skiptoinla"
  ),
  c(
    variable = "skipinla",
    default_value = "FALSE",
    description = "Logical: Skip the INLA fit? Useful to skip straight to predicting if a parallel_model was killed post-INLA fit but before predictions were saved."
  ),
  c(
    variable = "skiptoinla",
    default_value = "FALSE",
    description = "Logical: Skip past stacking and straight to INLA in parallel model? Used in conjunction with skiptoinla_from_run_date. If TRUE, stacking is skipped and stackers from skiptoinla_from_run_date will be used."
  ),
  c(
    variable = "skiptoinla_from_rundate",
    default_value = "NULL",
    description = "run_date string: This should be of the form \"2018_01_04_16_25_58\" (4:25:58 pm on January 4th, 2018). Used if skiptoinla==TRUE to load stacking results from older runs. Will attempt to copy the model image history file: /share/geospatial/mbg/<indicator_group>/<indicator>/model_image_history/<skiptoinla_from_run_date>_bin<age>_<reg>_<holdout>.RData into the model_image_history directory for the current run date and load it."
  ),
  c(
    variable = "slots",
    default_value = "10",
    description = "Integer: Number of slots to request for each parallel job. 10 slots on geos gets you ~10 cores & ~200gb RAM. 40 slots on prod gets you ~30 cores and ~80gb RAM. Must scale depending on size of problem and where you plan to launch the jobs."
  ),
  c(
    variable = "spat_strat",
    default_value = "qt",
    description = "String: TO BE FILLED IN"
  ),
  c(
    variable = "ss_col",
    default_value = "weighted_n",
    description = "String: Name of column to use while making holdouts using methods that use datapoint sample size"
  ),
  c(
    variable = "st_targ",
    default_value = "0.05",
    description = "Real: Target relevant to your indicator. e.g. WHO global nutrition targets state that by 2025 wasting should be less than 0.05*100%"
  ),
  c(
    variable = "stacked_fixed_effects",
    default_value = "gam + lasso + gbm",
    description = "String: Available stackers separated by spaces and plus signs. Dictates which stackers will be fit and available for use in INLA/TMB fit. Current available options are: gbm, gam, lasso, ridge, enet, xgboost"
  ),
  c(
    variable = "stackers_in_transform_space",
    default_value = "TRUE",
    description = "Logical: Should the stackers enter the INLA fit in transform space (e.g. if you want the stackers be in logit space when using a binomial model). This almost always should be TRUE unless you have good reason otherwise."
  ),
  c(
    variable = "subnat_country_to_get",
    default_value = "",
    description = "String: ISO3 code of the country to implement random effects on; CURRENTLY ONLY A SINGLE COUNTRY IS SUPPORTED"
  ),
  c(
    variable = "subnational_raking",
    default_value = "TRUE",
    description = "Logical: If TRUE, should subnational raking be performed in geogrpahies where GBD makes subnational estimates? We are mandated to keep this set to TRUE to align with GBD unless we have good reason to do otherwise."
  ),
  c(
    variable = "summstats",
    default_value = "c('mean', 'cirange', 'upper', 'lower')",
    description = "String of R vector notation containing strings of summary metrics. For each of these summary metrics, a summary raster object will be created in post-estimation by calculating the metric for each pixel across all draws. This object can also be passed to summarize_admins to create these metrics for each admin across admin draws. Possible values are all vectorized functions(!) since the summstats are simple passed to an apply() function over the cell/admin_pred rows. Common choices include: mean, median, lower, upper, cirange"
  ),
  c(
    variable = "target_type",
    default_value = "less",
    description = "String: 'less' or 'greater'. Describes which side of st_targ signifies sucess of goal. e.g. wasting less than 0.05, vaccination greater than 80."
  ),
  c(
    variable = "temp_strat",
    default_value = "prop",
    description = "String: TO BE FILLED IN"
  ),
  c(
    variable = "test",
    default_value = "FALSE",
    description = "Logical: If TRUE, only a subset of your data (test_pct percent of it) is used and samples defaults to 100."
  ),
  c(
    variable = "test_pct",
    default_value = "5",
    description = "Integer: Number 0-100. if test==TRUE, what percent of your data should be kept for model fitting. Useful for quick testing iterations."
  ),
  c(
    variable = "time_stamp",
    default_value = "TRUE",
    description = "Logical: TRUE (this tells the code to make a custom date-time stamp to save this run. It should always be true and we should delete this option)."
  ),
  c(
    variable = "transform",
    default_value = "inverse-logit",
    description = "String: naming type of transform to perform post-prediction to get back to the correct space you expect as an output. For binomial models this should be left as 'inverse-logit'"
  ),
  c(
    variable = "use_child_country_fes",
    default_value = "FALSE",
    description = "Logical: Should child country fixed effects be included in STACKING formulas (where applicable - at the moment this only works/makes sense in BRT)"
  ),
  c(
    variable = "use_geos_nodes",
    default_value = "TRUE",
    description = "Logical: Use the lbd-cluster-* nodes?"
  ),
  c(
    variable = "use_gp",
    default_value = "TRUE",
    description = "Logical: Include the space-time Gaussian Process (i.e. space-time correlated Matern error structure) in the INLA fit?"
  ),
  c(
    variable = "use_inla_country_fes",
    default_value = "FALSE",
    description = "Logical: Include country FIXED effects in INLA. SHOULD ONLY BE USED IF YOU HAVE DATA IN ALL COUNTRIES IN THE REGION BEING FIT."
  ),
  c(
    variable = "use_inla_country_res",
    default_value = "TRUE",
    description = "Logical: Use country RANDOM effects in the INLA fit?"
  ),
  c(
    variable = "use_inla_nugget",
    default_value = "TRUE",
    description = "Logical: Include a nugget effect (i.e. irreducible error) in the INLA fit?"
  ),
  c(
    variable = "use_nid_res",
    default_value = "FALSE",
    description = "Logical: add in NID random effects. CURRENTLY ONLY AVAILABLE IN TMB"
  ),
  c(
    variable = "use_raw_covs",
    default_value = "FALSE",
    description = "Logical: Use raw covariates (i.e. non-stacked geospatial and non-stacked gbd covs) in the INLA fit?"
  ),
  c(
    variable = "use_s2_mesh",
    default_value = "FALSE",
    description = "Logical: If true, the SPDE FEM mesh is generated on a spherical (S^2) manifold instead of on the flat plane (R^2). For large regions this is suggested. Furthermore, this config argument will generate a non-constant mesh where areas with less observed data will have larger triangles."
  ),
  c(
    variable = "use_share",
    default_value = "TRUE",
    description = "Logical: Use /share drive instead of J to load data and save all outputs. Strongly suggested to set to TRUE."
  ),
  c(
    variable = "use_stacking_covs",
    default_value = "TRUE",
    description = "Logical: Use stacking covariates in INLA?"
  ),
  c(
    variable = "use_subnat_res",
    default_value = "FALSE",
    description = "Logical: Use subnational random effects in INLA?"
  ),
  c(
    variable = "withtag",
    default_value = "FALSE",
    description = "Logical: Load a custom data matrix that is not named with the default indicator_group name?"
  ),
  c(
    variable = "year_list",
    default_value = "c(2000:2017)",
    description = "Vector of Integers: Vector consisting of all years for which we aim to produce predictions"
  ),
  c(
    variable = "yearload",
    default_value = "annual",
    description = "String: Determine if model is run annually ('annual') or in 5-year bins ('five-year'). Strongly suggested to run annually."
  ),
  c(
    variable = "yr_col",
    default_value = "year",
    description = "String: Name of column to use in making (space-)time holdouts"
  ),
  c(
    variable = "z_list",
    default_value = "0",
    description = "Ordered vector of items in your zcol. Ordering describes the relationship in among them. CURRENTLY ONLY AVAILABLE IN TMB"
  ),
  c(
    variable = "zcol",
    default_value = "z_column_default_blank",
    description = "String specifying name of column to use for 3rd dimension in Gaussian Process residual errors. CURRENTLY ONLY AVAILABLE IN TMB"
  )
), ncol = 3, byrow = T))

data.table::setnames(
  config_description,
  c("V1", "V2", "V3"),
  c("variable", "default", "description")
)

## SANITY CHECKS
# We want to make sure that all values appear in all configs and flag any
# that do not.

cfs <- unique(c(names(config_values), must_haves$V1, config_description$variable))

for (c in cfs) {
  if (!(c %in% names(config_values))) {
    cat(paste0(c, " is not in the config_values object. Please consider adding.\n"))
  }
  if (!(c %in% must_haves$V1)) {
    cat(paste0(c, " is not in the must_haves object. Please consider adding.\n"))
  }
  if (!(c %in% config_description$variable)) {
    cat(paste0(c, " is not in the config_description object. Please consider adding.\n"))
  }
}

## Finally save the objects to the data R file
# the expectation is that you are running this script within a Rproj
# which has lbd_core as the home directory. If this is not true you will need
# to change the r working directory manually.
save(must_haves, "./mbg_central/LBDCore/data/must_haves.rda")
save(config_description, "./mbg_central/LBDCore/data/config_description.rda")
save(config_values, "./mbg_central/LBDCore/data/config_values.rda")
