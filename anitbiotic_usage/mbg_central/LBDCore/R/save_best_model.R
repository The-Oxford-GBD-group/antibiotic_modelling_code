################ SAVE BEST MODEL FUNCTION ##########################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param measure PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname save_best_model
#' @export
save_best_model <- function(indicator_group, indicator, run_date, measure) {

  ######################################################################
  ######################################################################

  ## Set repo location and indicator group
  core_repo <- "/share/code/geospatial/lbd_core/"


  ## Load libraries and miscellaneous MBG project functions.
  mbg_functions <- c(
    "mbg_functions.R", "prep_functions.R",
    "covariate_functions.R", "misc_functions.R",
    "post_estimation_functions.R", "gbd_functions.R",
    "shiny_functions.R", "holdout_functions.R",
    "categorical_variable_functions.R",
    "validation_functions.R",
    "seegMBG_transform_functions.R"
  )
  source(paste0(core_repo, "/mbg_central/setup.R"))
  source_functions(paste(core_repo, "mbg_central", mbg_functions, sep = "/"))
  load_R_packages(c(
    "foreign", "rgeos", "data.table", "raster", "rgdal", "INLA",
    "seegSDM", "seegMBG", "plyr", "dplyr"
  ))

  ######################################################################
  ######################################################################

  ## load recentmodel runs
  raked <- brick(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_mean_raked_raster.tif"))
  unraked <- brick(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_mean_raster.tif"))

  ## extend my africa rasters
  raked <- extend(raked, extent(-180, 180, -90, 90), keepres = TRUE)
  unraked <- extend(unraked, extent(-180, 180, -90, 90), keepres = TRUE)

  ## load in a covariate to match my new global rasters to the same pixels
  ## Hard-code central directories
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")
  central_cov_dir <- paste0(root, "/WORK/11_geospatial/01_covariates/09_MBG_covariates/") # central folder per Lucas
  central_tv_covs <- c("evi", "lights_new", "LST_day", "total_pop", "rates", "malaria", "fertility", "urban_rural", "land_cover", "LST_avg", "gpcp_precip", "aridity_cruts")
  central_ntv_covs <- c("access", "irrigation", "LF", "LF_vector", "reservoirs", "aridity", "elevation", "annual_precip", "PET", "dist_rivers_lakes", "dist_rivers_only", "lat", "lon", "latlon")
  evi <- brick(paste0(central_cov_dir, "EVI_stack.tif"))

  ## make sure we match other covariates
  raked <- setExtent(raked, evi, keepres = TRUE, snap = TRUE)
  unraked <- setExtent(unraked, evi, keepres = TRUE, snap = TRUE)

  ## Make all dirs
  main_dir <- "/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/"
  dir.create(paste0(main_dir, indicator, "_raked/"))
  dir.create(paste0(main_dir, indicator, "_raked/", measure))
  dir.create(paste0(main_dir, indicator, "_raked/", measure, "/1y/"))
  dir.create(paste0(main_dir, indicator, "_unraked/"))
  dir.create(paste0(main_dir, indicator, "_unraked/", measure))
  dir.create(paste0(main_dir, indicator, "_unraked/", measure, "/1y/"))

  ## Write raster layer for each year, raked and unraked
  for (i in 1:length(names(unraked))) {
    subset_raked <- raked[[i]]
    subset_unraked <- unraked[[i]]
    year <- (i - 1) + 2000
    message(paste0("Saving ", year))
    writeRaster(subset_raked,
      filename = paste0("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/", indicator, "_raked/", measure, "/1y/", indicator, "_raked_", measure, "_1y_", year, "_00_00.tif"),
      format = "GTiff",
      overwrite = TRUE
    )
    writeRaster(subset_unraked,
      filename = paste0("/snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/", indicator, "_unraked/", measure, "/1y/", indicator, "_unraked_", measure, "_1y_", year, "_00_00.tif"),
      format = "GTiff",
      overwrite = TRUE
    )
  }

  message("All layers successfully saved in /snfs1/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/")
  message(paste0("Indicator ", indicator, " saved as ", indicator, "_raked and ", indicator, "_unraked to call in MBG config files in the fixed_effects parameter."))
}
