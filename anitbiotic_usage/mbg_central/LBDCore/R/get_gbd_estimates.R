#' @title Get GBD estimates
#' @description Loads national or subnational gbd estimates to be used for raking.
#'
#' @param gbd_name = GBD cov_name_short if "covariate" and GBD cause_id if "output"
#' @param region = name of region you want to pull results for. e.g. 'africa'
#' @param measure_id = if "output", which measure you want to pull (defaults to incidence)
#' @param age_group_id = if "output", which age group you want to pull (defaults to Under 5)
#' @param metric_id = if "output", which metric you want to pull (defaults to rate)
#' @param year_ids = numeric vector of years to pull
#' @param shapefile_version string of dated shapefile version to use when generating list of ihme loc ids and ADM code
#' @param rake_subnational Logical. do you want subnational estimates or just national ones
#' @param gbd_round_id numeric gbd round id to pull from
#'
#' @return Returns 3-column data.table where "name" = ihme loc id, "year" = year, and
#'      "mean" = value. Consistent with expected input for rake_cell_pred and calculate_raking_factors.
#'
#' @export
get_gbd_estimates <- function(gbd_type,
                              gbd_name,
                              region,
                              measure_id = 6,
                              age_group_id = 1,
                              metric_id = 3,
                              year_ids = c(2000:2017),
                              shapefile_version = "current",
                              rake_subnational = TRUE,
                              gbd_round_id = 5) {

  ## get GAUL to location_id mapping
  gaul_to_loc_id <- get_gbd_locs(
    reg = region,
    rake_subnational = rake_subnational,
    shapefile_version = shapefile_version
  )
  
  ## If we had subnational raking on, then we additionally pull in the national estimates
  ## because fractional rates raking needs it
  if(rake_subnational) {
    gaul_to_loc_id_nats <- get_gbd_locs(
      reg = region,
      rake_subnational = FALSE,
      shapefile_version = shapefile_version
    )
    
    gaul_to_loc_id <- rbindlist(list(as.data.table(gaul_to_loc_id)[, list(location_id, ADM_CODE)],
                                     as.data.table(gaul_to_loc_id_nats)), use.names = TRUE)
    
  } else {
    gaul_to_loc_id <- as.data.table(gaul_to_loc_id)[, list(location_id, ADM_CODE)]
  }
  
  gaul_to_loc_id <- unique(gaul_to_loc_id)
  loc_ids <- gaul_to_loc_id[, location_id]

  ## get cause metadata
  source(path_join(CC_ENV_DIR, "get_cause_metadata.R"))
  metadata <- get_cause_metadata(cause_set_id = 2, gbd_round_id = gbd_round_id)
  cause_id <- suppressWarnings(as.numeric(gbd_name))
  if (is.na(cause_id)) cause_id <- metadata[acause == gbd_name, cause_id]

  ## get cause data
  source(path_join(CC_ENV_DIR, "get_outputs.R"))
  gbd_estimates <- get_outputs(
    topic = "cause",
    version = "best",
    gbd_round_id = gbd_round_id,
    cause_id = cause_id,
    measure_id = measure_id,
    metric_id = metric_id,
    age_group_id = age_group_id,
    location_id = loc_ids,
    year_id = year_ids
  )

  all_data <- merge(gaul_to_loc_id, gbd_estimates, by = "location_id")
  setnames(all_data, c("location_id", "year_id", "val"), c("name", "year", "mean"))
  all_data <- all_data[, list(name, year, mean)]

  return(all_data)
}
