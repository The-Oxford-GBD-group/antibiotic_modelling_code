

# Preamble ----------------------------------------------------------------


print(Sys.time())

## Point to personal directory (create if needed)
personal_lib <- sprintf(
  "~/R/x86_64-pc-linux-gnu-library/%s.%sgeo/",
  R.Version()$major, R.Version()$minor
)
Sys.setenv(R_LIBS_USER = personal_lib)
if (!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)

## Set up .libPaths()
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

## Set up MKLROOT directory (needed if using RStudio)
Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")

## Load the LBDCore library
## The `suppressMessages()` is there just to remove all the intermediate
## packages' loading messages.
suppressMessages(library(LBDCore))

## Setup preambles
pipeline_preamble(headnode = FALSE)



# Final Pre-MBG Processing ------------------------------------------------


## If skipping to INLA, then just quit job
if (as.logical(DAG_obj$pipeline$config_list$skiptoinla)) {
  print("Skipping to INLA")

  ## Save out environment
  mbg_save_nodeenv(
    node = nodename,
    ig = indicator_group,
    indic = indicator,
    rd = run_date,
    reg = reg,
    age = age,
    holdout = holdout,
    objs = ls()
  )

  ## Create output file and remove err file ##
  mbg_job_marker(type = "end", tmpdir = "~/mbgdir")

  q("no")
}


## set the fixed effects to use in INLA based on config args
all_fixed_effects <- build_fixed_effects_logic(
  dagobject = DAG_obj,
  admin_codes = admin_code_raster
)

## copy things back over to df
df <- copy(the_data)

## remove the covariate columns so that there are no name conflicts
## when they get added back in
df <- df[, paste0(the_covs) := rep(NULL, length(the_covs))]

## Double-check that gaul codes get dropped before extracting
df <- df[, grep(
  "gaul_code_*", names(df),
  value = T
) := rep(
  NULL, length(grep("gaul_code_*", names(df), value = T))
)]

## create a full raster list to carry though to the shiny/next steps
if (DAG_obj$pipeline$config_list$use_stacking_covs) {
  cov_list <- c(unlist(stacked_rasters), unlist(all_cov_layers))
  child_mod_ras <- cov_list[child_model_names]
} else {
  cov_list <- unlist(all_cov_layers)
  child_model_names <- ""
}




## For raw covs, we don't want to center-scale
## (as that will happen in `build_mbg_data_stack()`)
##
## This is a bit weird, but when stacking covs are used the oos-stackers
## (used in `fit_mbg()`) do not get center scaled in save_mbg_input() - this
## just harmonizes the measures.  If this step isn't done, then the covs get
## double-center-scaled with odd results.
##
## For predict_mbg, non-center-scaled covs are pulled from cov_list (either
## stackers or raw) and center-scaled within the function.  So both fit and
## predict take place on center-scaled covs
##
## TODO: move all center-scaling to a single location to avoid these
## crazy acrobatics. But for now, we just do this.
if (as.logical(DAG_obj$pipeline$config_list$use_raw_covs) == TRUE) {
  centre_scale_covs <- FALSE
} else {
  centre_scale_covs <- TRUE
}



# Last Stages Prepping ----------------------------------------------------



just_covs <- extract_covariates(df, cov_list,
  return_only_results = T,
  centre_scale = centre_scale_covs,
  period_var = "year",
  period_map = period_map
)
if (centre_scale_covs == TRUE) {
  just_covs <- just_covs$covs
}
just_covs <- just_covs[, year := NULL]
just_covs <- just_covs[, period_id := NULL]
df <- cbind(df, just_covs)

# create a period column
if (is.null(period_map)) {
  period_map <- make_period_map(c(2000, 2005, 2010, 2015))
}
df[, period := NULL]
setnames(period_map, "data_period", "year")
setnames(period_map, "period_id", "period")
df <- merge(df, period_map, by = "year", sort = F)

## Now that we've extracted covariate values to our data in the buffer zone,
## clip cov list to simple_raster instead of simple_polygon
##    (simple_raster is area we actually want to model over)
for (l in 1:length(cov_list)) {
  cov_list[[l]] <- crop(cov_list[[l]], extent(simple_raster))
  cov_list[[l]] <- setExtent(cov_list[[l]], simple_raster)
  cov_list[[l]] <- raster::mask(cov_list[[l]], simple_raster)
}
rm(l)




## Build spatial mesh over modeling area
mesh_s <- build_space_mesh(
  d = df,
  simple = simple_polygon,
  max_edge = DAG_obj$pipeline$config_list$mesh_s_max_edge,
  mesh_offset = DAG_obj$pipeline$config_list$mesh_s_offset,
  s2mesh = DAG_obj$pipeline$config_list$use_s2_mesh,
  
  ## s2params MUST be in stringed vector format. Yuck.
  s2params = DAG_obj$pipeline$config[V1 == "s2_mesh_params", V2]
)

## Build temporal mesh (standard for now)
if (length(DAG_obj$pipeline$config_list$year_list) == 1) {
  mesh_t <- NULL
} else {
  mesh_t <- build_time_mesh(
    periods = eval(
      parse(
        text = DAG_obj$pipeline$config_list$mesh_t_knots
      )
    )
  )
}




# Postamble ---------------------------------------------------------------


pipeline_postamble(addl_objs_to_save = c(
  "df", "simple_raster", "mesh_s", "mesh_t",
  "cov_list", "child_model_names", "all_fixed_effects",
  "period_map"
))


q("no")
