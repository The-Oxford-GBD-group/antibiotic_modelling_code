
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



# Stacking ----------------------------------------------------------------



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


tic("Stacking - all") ## Start stacking master timer

## Figure out which models we're going to use
child_model_names <- DAG_obj$pipeline$config_list$stacked_fixed_effects %>%
  gsub(" ", "", .) %>%
  strsplit(., "+", fixed = T) %>%
  unlist()
message(paste0(
  "Child stackers included are: ",
  paste(child_model_names,
    collapse = " // "
  )
))

the_covs <- format_covariates(all_fixed_effects)

## copy the dataset to avoid unintended namespace conflicts
the_data <- copy(df)

## shuffle the data into six folds
the_data <- the_data[base::sample(nrow(the_data)), ]
the_data[, fold_id := cut(
  seq(1, nrow(the_data)),
  breaks = as.numeric(
    DAG_obj$pipeline$config_list$n_stack_folds
  ),
  labels = FALSE
)]

## add a row id column
the_data[, a_rowid := seq(1:nrow(the_data))]

## extract covariates to the points and subset data
## where its missing covariate values
cs_covs <- extract_covariates(the_data,
  all_cov_layers,
  id_col = "a_rowid",
  return_only_results = TRUE,
  centre_scale = TRUE,
  period_var = "year",
  period_map = period_map
)

# A check to see if any of the variables do not vary across the data.
# This could break model later so we check and update some objects
covchecklist <- check_for_cov_issues(
  cc = cs_covs,
  afe = all_fixed_effects,
  afeb = all_fixed_effects_brt,
  fe = DAG_obj$pipeline$config_list$fixed_effects,
  check_pixelcount = DAG_obj$pipeline$config_list$check_cov_pixelcount,
  check_pixelcount_thresh = ifelse(
    "pixelcount_thresh" %in% names(DAG_obj$pipeline$config_list),
    DAG_obj$pipeline$config_list$pixelcount_thresh, 0.95
  )
)
for (n in names(covchecklist)) {
  assign(n, covchecklist[[n]])
}

# Plot covariates as a simple diagnostic here
pdf(sprintf(
  "%s/raw_covariates_%s.pdf",
  DAG_obj$pipeline$outputdir,
  DAG_obj$pathaddin
), height = 12, width = 12)
for (covname in names(all_cov_layers)) {
  plot(all_cov_layers[[covname]], main = covname, maxpixel = 1e6)
}
dev.off()

## Check for data where covariate extraction failed
rows_missing_covs <- nrow(the_data) - nrow(cs_covs[[1]])
if (rows_missing_covs > 0) {
  pct_missing_covs <- round((rows_missing_covs / nrow(the_data)) * 100, 2)
  warning(
    paste0(
      rows_missing_covs, " out of ", nrow(the_data), " rows of data ",
      "(", pct_missing_covs, "%) do not have corresponding ",
      "covariate values and will be dropped from child models..."
    )
  )
  if (rows_missing_covs / nrow(the_data) > 0.1) {
    stop(
      paste0(
        "Something has gone quite wrong: more than 10% of your data ",
        " does not have corresponding covariates.  You should investigate ",
        "this before proceeding."
      )
    )
  }
}

the_data <- merge(the_data, cs_covs[[1]], by = "a_rowid", all.x = F, all.y = F)

## store the centre scaling mapping
covs_cs_df <- cs_covs[[2]]

## this will drop rows with NA covariate values
the_data <- na.omit(the_data, c(indicator, "N", the_covs))

## stop if this na omit demolished the whole dataset
if (nrow(the_data) == 0) {
  stop(paste0(
    "You have an empty df, make sure one of your",
    " covariates was not NA everywhere."
  ))
}


## Running Stackers
if (DAG_obj$pipeline$config_list$use_stacking_covs) {
  message("Fitting Stackers")

  # Run the child stacker models
  child_model_run <- run_child_stackers(
    models = child_model_names,
    input_data = the_data,
    indicator = DAG_obj$pipeline$indicator,
    indicator_family = DAG_obj$pipeline$config_list$indicator_family,
    covariates_nonbrt = all_fixed_effects,
    covariates_brt = all_fixed_effects_brt,
    outputdir = DAG_obj$pipeline$outputdir,
    reg = DAG_obj$reg
  )

  # Bind the list of predictions into a data frame
  child_mods_df <- do.call(cbind, lapply(child_model_run, function(x) x[[1]]))

  ## combine the children models with the_data
  the_data <- cbind(the_data, child_mods_df)

  ## Rename the child model objects into a named list
  child_model_objs <- setNames(
    lapply(child_model_run, function(x) x[[2]]),
    child_model_names
  )



  ## return the stacked rasters
  stacked_rasters <- make_stack_rasters(
    covariate_layers = all_cov_layers, # raster layers and bricks
    period = min(period_map[, period_id]):max(period_map[, period_id]),
    child_models = child_model_objs,
    indicator_family = DAG_obj$pipeline$config_list$indicator_family,
    centre_scale_df = covs_cs_df
  )

  ## plot stackers
  pdf(paste0(
    DAG_obj$pipeline$outputdir,
    "/stacker_rasters",
    DAG_obj$pathaddin, ".pdf"
  ))
  for (i in 1:length(stacked_rasters)) {
    plot(stacked_rasters[[i]],
      main = names(stacked_rasters[[i]]),
      maxpixel = ncell(stacked_rasters[[i]])
    )
  }
  dev.off()

  message("Stacking is complete")
}


# Postamble ---------------------------------------------------------------


pipeline_postamble(addl_objs_to_save = "the_data")
q("no")
