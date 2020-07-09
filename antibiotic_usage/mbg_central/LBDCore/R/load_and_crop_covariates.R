## Load and crop covariates to the modeled area
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fixed_effects PARAM_DESCRIPTION
#' @param simple_polygon PARAM_DESCRIPTION
#' @param agebin PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_and_crop_covariates
#' @export
load_and_crop_covariates <- function(fixed_effects, simple_polygon, agebin = 1) {

  # get selected covs
  selected_covs <- strsplit(fixed_effects, " ")
  selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]

  # Hard-code central directories
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")
  central_cov_dir <- paste0(root, "/WORK/11_geospatial/01_covariates/09_MBG_covariates/") # central folder per Lucas
  central_tv_covs <- c("mss", "msw", "unrakedmss", "unrakedmsw", "sevwaste", "sevstunt", "matedu_yrs", "unrakedmatedu_yrs", "wocba", "evi", "lights_new", "LST_day", "total_pop", "rates", "malaria", "fertility", "fertility_infill", "fertility_smooth", "urban_rural", "land_cover", "LST_avg", "gpcp_precip", "aridity_cruts", "malaria_pfpr")
  central_ntv_covs <- c("access", "irrigation", "LF", "LF_vector", "reservoirs", "aridity", "elevation", "annual_precip", "PET", "dist_rivers_lakes", "dist_rivers_only", "lat", "lon", "latlon")

  # Load all temporally-varying covariates
  evi <- brick(paste0(central_cov_dir, "EVI_stack.tif"))
  lights_new <- brick(paste0(central_cov_dir, "NTL_stack.tif"))
  LST_day <- brick(paste0(central_cov_dir, "LST_day_stack.tif"))
  total_pop <- brick(paste0(central_cov_dir, "WorldPop_allStages_stack.tif"))
  urban_rural <- brick(paste0(central_cov_dir, "GHS_settlement_model_stack.tif"))
  land_cover <- brick(paste0(central_cov_dir, "landCover_stack.tif"))
  LST_avg <- brick(paste0(central_cov_dir, "LST_avg_stack.tif"))
  gpcp_precip <- brick(paste0(central_cov_dir, "GPCP_precip_stack.tif"))
  aridity_cruts <- brick(paste0(central_cov_dir, "cruts_ard_stack.tif"))

  # Load all temporally-nonvarying covariates
  # Human/Cultural Synoptic Rasters
  access <- brick(paste0(central_cov_dir, "synoptic_humCul_stack.tif"))$synoptic_humCul_stack.1
  irrigation <- brick(paste0(central_cov_dir, "synoptic_humCul_stack.tif"))$synoptic_humCul_stack.2
  LF <- brick(paste0(central_cov_dir, "synoptic_humCul_stack.tif"))$synoptic_humCul_stack.3
  LF_vector <- brick(paste0(central_cov_dir, "synoptic_humCul_stack.tif"))$synoptic_humCul_stack.4
  reservoirs <- brick(paste0(central_cov_dir, "synoptic_humCul_stack.tif"))$synoptic_humCul_stack.5
  # Environmental/Physical Synoptic Rasters
  aridity <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.1
  dist_rivers_lakes <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.2
  dist_rivers_only <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.3
  elevation <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.4
  annual_precip <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.5
  PET <- brick(paste0(central_cov_dir, "synoptic_envPhy_stack.tif"))$synoptic_envPhy_stack.6

  lat <- raster(paste0(central_cov_dir, "lat.tif"))
  lon <- raster(paste0(central_cov_dir, "lon.tif"))
  latlon <- lat * lon


  # some u5m additions
  malaria <- brick(paste0(central_cov_dir, "malaria_infant_death_rate_stack.tif"))
  if ("malaria" %in% selected_covs) values(malaria) <- log(as.matrix(malaria) + .01)
  fertility <- brick(paste0(central_cov_dir, "fertility_stack.tif"))
  fertility_smooth <- brick(paste0(central_cov_dir, "fertility_smooth_stack.tif"))
  fertility_infill <- brick(paste0(central_cov_dir, "fertility_infill_stack.tif"))

  mss <- brick(paste0(central_cov_dir, "mss_stack.tif"))
  msw <- brick(paste0(central_cov_dir, "msw_stack.tif"))
  unrakedmss <- brick(paste0(central_cov_dir, "unrakedmss_stack.tif")) #' unrakedmss_stack.tif'))
  unrakedmsw <- brick(paste0(central_cov_dir, "unrakedmsw_stack.tif")) #' unrakedmsw_stack.tif'))
  sevstunt <- brick(paste0(central_cov_dir, "ss_stack.tif")) #' sevstunt_stack.tif'))
  sevwaste <- brick(paste0(central_cov_dir, "sw_stack.tif")) #' sevwaste_stack.tif'))

  wocba <- brick(paste0(central_cov_dir, "WOCBA_stack.tif"))
  malaria_pfpr <- brick(paste0(central_cov_dir, "malaria_pfpr_stack.tif"))

  matedu_yrs <- brick(paste0(central_cov_dir, "matedu_yrs_stack.tif")) #' matedu_yrs_stack.tif'))
  unrakedmatedu_yrs <- brick(paste0(central_cov_dir, "unrakedmatedu_yrs_stack.tif")) #' unrakedmatedu_yrs_stack.tif'))


  u5m_dir <- paste0(root, "/temp/geospatial/U5M_africa/")
  load(paste0(u5m_dir, "data/raw/covariates/national_mr_m0.Rdata"))
  load(paste0(u5m_dir, "data/raw/covariates/national_mr_m1_11.Rdata"))
  load(paste0(u5m_dir, "data/raw/covariates/national_mr_2q1.Rdata"))
  load(paste0(u5m_dir, "data/raw/covariates/national_mr_2q3.Rdata"))
  load(paste0(u5m_dir, "data/raw/covariates/national_mr_5q0.Rdata"))
  rates <- get(paste0("rates_", agebin))
  names(rates) <- paste0("rates.", 1:4)
  if ("rates" %in% selected_covs) rates <- extend(rates, extent(-180, 180, -90, 90), keepres = TRUE)

  # Add names to layersss
  names(access) <- "access"
  names(irrigation) <- "irrigation"
  names(LF) <- "LF"
  names(LF_vector) <- "LF_vector"
  names(reservoirs) <- "reservoirs"
  names(aridity) <- "aridity"
  names(elevation) <- "elevation"
  names(annual_precip) <- "annual_precip"
  names(PET) <- "PET"
  names(dist_rivers_only) <- "dist_rivers_only"
  names(dist_rivers_lakes) <- "dist_rivers_lakes"
  names(lat) <- "lat"
  names(lon) <- "lon"
  names(latlon) <- "latlon"


  for (c in central_tv_covs) {
    tmp <- get(c)
    names(tmp) <- rep(paste0(c, ".", 1:4))
    assign(c, tmp)
  }

  # Construct list of covariates to GAM and use in model from fixed_effects parameter equation.
  num_covs <- length(selected_covs)
  lcovs <- list()
  for (i in 1:num_covs) {
    message(selected_covs[i])
    this_cov <- selected_covs[i]
    if (this_cov %in% central_ntv_covs) { # Add if it is from the temporally non-varying list.
      lcovs[[i]] <- get(this_cov)
    }
    if (this_cov %in% central_tv_covs) { # Add if it is from the temporally varying list.
      lcovs[[i]] <- get(this_cov)
    }
    names(lcovs)[i] <- this_cov
  }

  # Make sure covariate layers line up with raster we are modeling over
  for (l in 1:length(lcovs)) {
    message(names(lcovs)[l])
    print(lcovs[[l]])
    lcovs[[l]] <- crop(lcovs[[l]], extent(simple_polygon))
    print(lcovs[[l]])
    lcovs[[l]] <- setExtent(lcovs[[l]], simple_polygon) # , keepres=TRUE, snap=TRUE)
    print(lcovs[[l]])
    lcovs[[l]] <- mask(lcovs[[l]], simple_polygon)
  }

  return(lcovs)
}
