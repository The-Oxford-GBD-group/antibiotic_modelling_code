#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gbd_type PARAM_DESCRIPTION
#' @param gbd_name PARAM_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param measure_id PARAM_DESCRIPTION, Default: 6
#' @param age_group_id PARAM_DESCRIPTION, Default: 1
#' @param metric_id PARAM_DESCRIPTION, Default: 3
#' @param year_ids PARAM_DESCRIPTION, Default: c(2000:2017)
#' @param return_by_age_sex PARAM_DESCRIPTION, Default: 'no'
#' @param collapse_age_sex PARAM_DESCRIPTION, Default: FALSE
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @param gbd_round_id PARAM_DESCRIPTION, Default: 5
#' @param named_location_field PARAM_DESCRIPTION, Default: 'GAUL_CODE'
#' @param ... any other fields to be passed to all of the get_* functions from GBD CC
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_gbd_data
#' @export
load_gbd_data <- function(gbd_type,
                          gbd_name,
                          gaul_list,
                          measure_id = 6,
                          age_group_id = 1,
                          metric_id = 3,
                          year_ids = c(2000:2017),
                          return_by_age_sex = "no",
                          collapse_age_sex = FALSE,
                          shapefile_version = "current",
                          gbd_round_id = 5,
                          named_location_field = "GAUL_CODE",
                          ...) {

  #################################################################################
  ### Pull GBD estimates from database and return as a data table
  ## Inputs:
  ##    gbd_type = "covariate" or "output", depending which database you need to pull from
  ##    gbd_name = GBD cov_name_short if "covariate" and GBD cause_id if "output"
  ##    gaul_list = list of GAUL codes you want to pull
  ##    measure_id = if "output", which measure you want to pull (defaults to incidence)
  ##    age_group_id = if "output", which age group you want to pull (defaults to Under 5)
  ##    metric_id = if "output", which metric you want to pull (defaults to rate)
  ##    named_location_field: string specifying which name of numerical location identifier that should be returned in 'name' column. could be 'GAUL_CODE' or 'location_id'
  ## Outputs:
  ##    Returns 3-column data.table where "name" = (usually) GAUL code, "year" = year, and
  ##      "mean" = value. Consistent with expected input for calc_raking_factors.
  #################################################################################

  # get GAUL to location_id mapping
  gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
  gaul_to_loc_id <- gaul_to_loc_id[, list(location_id = loc_id, GAUL_CODE)]

  loc_ids <- gaul_to_loc_id[GAUL_CODE %in% gaul_list, location_id]

  # load covariate data
  if (gbd_type == "covariate") {

    # get covariate metadata from covariate_name_short
    metadata <- get_covariate_metadata()
    covariate_id <- metadata[covariate_name_short == tolower(gbd_name), covariate_id]
    covariate_by_age <- metadata[covariate_name_short == tolower(gbd_name), by_age]
    covariate_by_sex <- metadata[covariate_name_short == tolower(gbd_name), by_sex]

    # get covariate data
    source(path_join(CC_ENV_DIR, "get_covariate_estimates.R"))
    if (covariate_by_age) {
      gbd_estimates <- get_covariate_estimates(covariate_id = covariate_id, location_id = loc_ids, year_id = year_ids, age_group_id = age_group_id, gbd_round_id = gbd_round_id, ...)
    } else {
      gbd_estimates <- get_covariate_estimates(covariate_id = covariate_id, location_id = loc_ids, year_id = year_ids, gbd_round_id = gbd_round_id, ...)
    }

    # collapse to all age, both sexes (if specified, and if there are multiple age and/or sex groups)
    if (collapse_age_sex & gbd_estimates[, uniqueN(age_group_name) > 1 | uniqueN(sex_id) > 1]) {

      # get population data
      source(path_join(CC_ENV_DIR, "get_population.R"))
      gbd_pops <- get_population(
        age_group_id = gbd_estimates[, unique(age_group_id)],
        location_id = gbd_estimates[, unique(location_id)],
        year_id = gbd_estimates[, unique(year_id)],
        sex_id = gbd_estimates[, unique(sex_id)],
        gbd_round_id = gbd_round_id,
        ...
      )

      # population-weight the covariate data
      gbd_estimates <- merge(gbd_estimates, gbd_pops, by = c("location_id", "sex_id", "age_group_id", "year_id"))
      gbd_estimates <- gbd_estimates[, list(mean_value = weighted.mean(mean_value, population, na.rm = T)), by = "location_id,year_id"]
    }

    # format and return
    gbd_estimates <- merge(gbd_estimates, gaul_to_loc_id, by = "location_id")
    setnames(gbd_estimates, c(named_location_field, "year_id", "mean_value"), c("name", "year", "mean"))

    if (return_by_age_sex == "no") gbd_estimates <- gbd_estimates[, list(name, year, mean)]
    if (return_by_age_sex == "yes") gbd_estimates <- gbd_estimates[, list(name, year, mean, sex_id, age_group_id)]

    return(gbd_estimates)
  }

  # load cause data
  if (gbd_type == "output") {

    # get cause metadata
    source(path_join(CC_ENV_DIR, "get_cause_metadata.R"))
    metadata <- get_cause_metadata(cause_set_id = 2, gbd_round_id = gbd_round_id)
    cause_id <- suppressWarnings(as.numeric(gbd_name))
    if (is.na(cause_id)) cause_id <- metadata[acause == gbd_name, cause_id]

    # get cause data
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
      year_id = year_ids,
      ...
    )

    all_data <- merge(gaul_to_loc_id, gbd_estimates, by = "location_id")
    setnames(all_data, c(named_location_field, "year_id", "val"), c("name", "year", "mean"))
    all_data <- all_data[, list(name, year, mean)]

    return(all_data)
  }
}
