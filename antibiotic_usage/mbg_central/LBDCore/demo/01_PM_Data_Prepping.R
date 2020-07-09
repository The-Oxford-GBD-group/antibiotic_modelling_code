
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
#### Nothing to do with parent nodes, because this is the first node in DAG
#### And so we set headnode = TRUE
pipeline_preamble(headnode = TRUE)




# Prep MBG inputs/Load Data -----------------------------------------------



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

## Load simple polygon template to model over
gaul_list <- get_adm0_codes(
  reg,
  shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version
)
simple_polygon_list <- load_simple_polygon(
  gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
  shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version
)
subset_shape <- simple_polygon_list[[1]]
simple_polygon <- simple_polygon_list[[2]]

## Load list of raster inputs (pop and simple)
raster_list <- build_simple_raster_pop(
  subset_shape,
  link_table = DAG_obj$pipeline$config_list$modeling_shapefile_version
)
simple_raster <- raster_list[["simple_raster"]]

## Load input data based on stratification and holdout,
## OR pull in data as normal and run with the whole dataset if holdout == 0.
## For holdouts, we have depreciated val level,
## so each val level must be recorded in a different run date
if (holdout != 0) {
  message(paste0(
    "Holdout != 0 so loading holdout data only from holdout ", holdout
  ))
  message(
    "Please be sure you have a list object called stratum_ho in your environment."
  )
  ## if stratifies by age then make sure loads correctly
  if (age != 0) {
    df <- as.data.table(
      stratum_ho[[paste("region", reg, "_age", age, sep = "__")]]
    )
  } else {
    df <- as.data.table(
      stratum_ho[[paste("region", reg, sep = "__")]]
    )
  }
  df <- df[fold != holdout, ]
} else {
  message("Holdout == 0 so loading in full dataset using load_input_data()")
  df <- load_input_data(
    indicator = gsub(paste0("_age", age), "", indicator),
    simple = simple_polygon,
    agebin = age,
    removeyemen = FALSE,
    date = run_date,
    pathaddin = DAG_obj$pathaddin,
    years = DAG_obj$pipeline$config_list$yearload,
    withtag = as.logical(DAG_obj$pipeline$config_list$withtag),
    datatag = DAG_obj$pipeline$config_list$datatag,
    use_share = as.logical(DAG_obj$pipeline$config_list$use_share),
    yl = DAG_obj$pipeline$config_list$year_list
  )
}

## if testing, we only keep test_pct of observations
if (test == 1) {
  test_pct <- as.numeric(DAG_obj$pipeline$config_list$test_pct)

  message(paste0(
    "Test option was set on and the test_pct argument was found at ",
    test_pct,
    "% \n\n                 ... keeping only ",
    round(nrow(df) * (test_pct / 100), 0), " random rows of data."
  ))
  df <- df[sample(nrow(df), round(nrow(df) * (test_pct / 100), 0)), ]

  message("Also, making it so we only take 100 draws")
  samples <- 100
}

## for u5m in particular,  make sure indicator
## is properly named here, wont affect others
df[[indicator]] <- df[[gsub(paste0("_age", age), "", indicator)]]

## if there is another weight column, multiply it with weight now
if (DAG_obj$pipeline$config_list$other_weight != "") {
  message(paste0(
    "Multiplying weight and ",
    DAG_obj$pipeline$config_list$other_weight
  ))
  df[["weight"]] <- df[["weight"]] * df[[other_weight]]
}


## Some built in data checks that cause known problems later on
if (
  DAG_obj$pipeline$config_list$indicator_family == "binomial" & any(
    df[, get(indicator)] / df$N > 1
  )) {
  stop("You have binomial data where k > N. Check your data before proceeding")
}
if (any(df[["weight"]] %in% c(Inf, -Inf) | any(is.na(df[["weight"]])))) {
  stop(
    "You have illegal weights (NA,Inf,-Inf). Check your data before proceeding"
  )
}





# Pull Covariates ---------------------------------------------------------


## Define modeling space. In years only for now.
if (DAG_obj$pipeline$config_list$yearload == "annual") {
  period_map <-
    make_period_map(
      modeling_periods = c(
        min(
          DAG_obj$pipeline$config_list$year_list
        ):max(
          DAG_obj$pipeline$config_list$year_list
        )
      )
    )
}
if (DAG_obj$pipeline$config_list$yearload == "five-year") {
  period_map <-
    make_period_map(
      modeling_periods = seq(
        min(
          DAG_obj$pipeline$config_list$year_list
        ), max(
          DAG_obj$pipeline$config_list$year_list
        ),
        by = 5
      )
    )
}

## Make placeholders for covariates
cov_layers <- gbd_cov_layers <- NULL

## Pull all covariate bricks/layers
if (nrow(DAG_obj$pipeline$fixed_effects_config) > 0) {
  message("Grabbing raster covariate layers")
  loader <- MbgStandardCovariateLoader$new(
    start_year = min(DAG_obj$pipeline$config_list$year_list),
    end_year = max(DAG_obj$pipeline$config_list$year_list),
    interval = as.numeric(DAG_obj$pipeline$config_list$interval_mo),
    covariate_config = DAG_obj$pipeline$fixed_effects_config
  )
  cov_layers <- loader$get_covariates(simple_polygon)
}

## Pull country level gbd covariates
if (nchar(DAG_obj$pipeline$config_list$gbd_fixed_effects) > 0) {
  message("Grabbing GBD covariates")

  gbd_cov_layers <- load_gbd_covariates(
    covs = trim(
      strsplit(DAG_obj$pipeline$config_list$gbd_fixed_effects, "\\+")[[1]]
    ),
    measures = trim(
      strsplit(
        DAG_obj$pipeline$config_list$gbd_fixed_effects_measures, "\\+"
      )[[1]]
    ),
    year_ids = DAG_obj$pipeline$config_list$year_list,
    age_ids = DAG_obj$pipeline$config_list$gbd_fixed_effects_age,
    template = cov_layers[[1]][[1]],
    simple_polygon = simple_polygon,
    interval_mo = DAG_obj$pipeline$config_list$interval_mo,
    shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version
  )
}

## Combine all covariates
all_cov_layers <- c(cov_layers, gbd_cov_layers)

## regenerate all fixed effects equation from the cov layers
all_fixed_effects <- paste(names(all_cov_layers), collapse = " + ")

## Make stacker-specific formulas where applicable
all_fixed_effects_brt <- all_fixed_effects

## Set Up Country Fixed Effects
if (DAG_obj$pipeline$config_list$use_child_country_fes == TRUE | DAG_obj$pipeline$config_list$use_inla_country_fes == TRUE) {
  message("Setting up country fixed effects")
  fe_gaul_list <- unique(c(
    gaul_convert(unique(df[, country]),
      shapefile_version =
        DAG_obj$pipeline$config_list$modeling_shapefile_version
    ),
    gaul_list
  ))
  fe_template <- cov_layers[[1]][[1]]
  simple_polygon_list <- load_simple_polygon(
    gaul_list = fe_gaul_list,
    buffer = 0.4,
    subset_only = TRUE,
    shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version
  )
  fe_subset_shape <- simple_polygon_list[[1]]
  admin_code_raster <- rasterize_check_coverage(
    fe_subset_shape, fe_template,
    field = "ADM0_CODE"
  )
  admin_code_raster <- setNames(admin_code_raster, "gaul_code")
  admin_code_raster <- create_categorical_raster(admin_code_raster)

  ## update covlayers and add country fixed effects to the formula object
  all_cov_layers <- update_cov_layers(all_cov_layers, admin_code_raster)
  all_fixed_effects_cfes <- paste(all_fixed_effects,
    paste(names(admin_code_raster)[1:length(names(admin_code_raster))],
      collapse = " + "
    ),
    sep = " + "
  )

  ## update specific stacker formulas
  ## For now we just want country effects in BRT
  all_fixed_effects_brt <- all_fixed_effects_cfes
} else {
  admin_code_raster <- NULL
}

## Add these to the fixed effects if we want them in stacking
if (DAG_obj$pipeline$config_list$use_child_country_fes == TRUE) {
  gaul_fes <- paste(
    names(admin_code_raster)[2:length(names(admin_code_raster))],
    collapse = " + "
  )
  all_fixed_effects <- paste(all_fixed_effects, gaul_fes, sep = " + ")
}




# Postamble ---------------------------------------------------------------


pipeline_postamble(headnode = TRUE)

q("no")
