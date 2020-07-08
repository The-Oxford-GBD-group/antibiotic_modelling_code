#' @title Custom Raking Function
#'
#' @description Builds a new simple polygon and raster, crosswalks the cell pred object to match the new raster. Generates population weights based on the shapefile and field passed in, calculates raking factors using the rake_to df
#' @param cell_pred Cell pred object to be raked.
#' @param shapefile_path Path to shapefile that will be used for
#'   raking.
#' @param field Field in shapefile that has admin identifiers that
#'   match with rake_to.
#' @param rake_to Df with name, year, and mean columns. values in name
#'   must match up with values in field.
#' @param reg Region used to produce cell pred object.
#' @param year_list List of years
#' @param modeling_shapefile_version string indicating which version of
#'   shapefile to use for matching cell_pred pixels to adm codes
#'
#' @return Returns a new cell pred object, simple raster, raking factors, pre-raking aggregate numbers, and rasters of mean, lower, upper, and cirange for years in year list
#' @export
#'
#' @examples
#' \dontrun{
#' custom_rake_output <- custom_rake(
#'   cell_pred = cell_draws,
#'   shapefile_path = "/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/IND_for_collaborator.shp",
#'   field = "gbd_loc_id",
#'   rake_to = rake_to,
#'   reg = "south_asia",
#'   year_list = c(2000:2015)
#' )
#' }
custom_rake <- function(cell_pred, shapefile_path, field, rake_to, reg, year_list,
                        modeling_shapefile_version = "current") {

  # initialize results list
  outputlist <- list()

  # Calculate interval month, number of months between years in year list
  year_diff <- diff(year_list)
  if (length(unique(year_diff)) != 1) {
    stop("Please use annual or 5-year intervals exclusively in year_list")
  } else {
    interval_mo <- year_diff[[1]] * 12
  }

  ## get simple polygon and simple raster used to produce cell pred
  message("Loading simple polygon")
  simple_polygon <- load_simple_polygon(
    gaul_list = get_adm0_codes(reg, shapefile_version = modeling_shapefile_version),
    buffer = 0.4,
    shapefile_version = modeling_shapefile_version
  )
  subset_shape <- simple_polygon[["subset_shape"]]
  simple_polygon <- simple_polygon[["spoly_spdf"]]

  message("Loading simple raster")
  raster_list <- build_simple_raster_pop(subset_shape)
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]

  # get new simple polygon and simple raster that will be raked to
  message("Loading new simple polygon to be raked to")
  new_simple_polygon <- load_simple_polygon(gaul_list = get_adm0_codes(reg), buffer = 0.4, custom_shapefile_path = shapefile_path)
  new_subset_shape <- new_simple_polygon[["subset_shape"]]
  new_simple_polygon <- new_simple_polygon[["spoly_spdf"]]

  message("Loading new simple raster to be raked to")
  new_raster_list <- build_simple_raster_pop(new_subset_shape)
  new_simple_raster <- new_raster_list[["simple_raster"]]
  new_pop_raster <- new_raster_list[["pop_raster"]]

  # get extents of original and simple raster to line up - extend and crop just in case
  new_simple_raster <- extend(new_simple_raster, simple_raster, values = NA)
  new_simple_raster <- crop(new_simple_raster, extent(simple_raster))

  # check original and new simple rasters match in extent and resolution
  if (extent(new_simple_raster) != extent(simple_raster)) {
    stop("new simple raster extent does not match original simple raster")
  }
  if (any(res(new_simple_raster) != res(simple_raster))) {
    stop("new simple raster resolution does not match original simple raster")
  }

  # crosswalk cell_pred to new raster and rename outputs
  message("Crosswalking cell pred object to new raster")
  new_pred_object <- crosswalk_cell_pred_add_NA(simple_raster, new_simple_raster, cell_pred, year_list)
  cell_pred <- new_pred_object[[1]]
  simple_raster <- new_pred_object[[2]]

  message("Getting population raster")
  ## Pull 2000-2015 annual population brick using new covariates function
  if (class(year_list) == "character") year_list <- eval(parse(text = year_list))
  pop_raster_annual <- load_and_crop_covariates_annual(
    covs = "worldpop",
    measures = pop_measure, # from config
    simple_polygon = simple_polygon,
    start_year = min(year_list),
    end_year = max(year_list),
    interval_mo = as.numeric(interval_mo),
    agebin = 1
  )[[1]]

  ## extend and crop pop raster to ensure it matches the simple raster
  pop_raster_annual <- extend(pop_raster_annual, simple_raster, values = NA)
  pop_raster_annual <- crop(pop_raster_annual, extent(simple_raster))
  pop_raster_annual <- setExtent(pop_raster_annual, simple_raster)
  pop_raster_annual <- mask(pop_raster_annual, simple_raster)

  ## check to ensure the pop raster matches the simple raster in extent and resolution
  if (extent(pop_raster_annual) != extent(simple_raster)) {
    stop("population raster extent does not match simple raster")
  }
  if (any(res(pop_raster_annual) != res(simple_raster))) {
    stop("population raster resolution does not match simple raster")
  }

  # get custom admin raster
  custom_admin_raster <- load_custom_admin_raster(shapefile_path, field, simple_raster)

  ## Create population weights using the annual brick and feed custom year argument to aggregation function
  message("Building population weights object")
  pop_wts_adm0 <- make_population_weights(
    admin_level = 0,
    simple_raster = simple_raster,
    pop_raster = pop_raster_annual,
    gaul_list = get_adm0_codes(reg),
    custom_admin_raster = custom_admin_raster
  )

  message("Making condSim")
  cond_sim_draw_adm0 <- make_condSim(
    admin_level = 0,
    pop_wts_object = pop_wts_adm0,
    cell_pred = cell_pred,
    gaul_list = get_adm0_codes(reg),
    summarize = FALSE,
    years = year_list
  )

  # save some raw country estimates to compare with GBD in a plot later on
  cond_sim_raw_adm0 <- apply(cond_sim_draw_adm0, 1, mean)
  adm0_geo <- cbind(
    mean = cond_sim_raw_adm0,
    lower = apply(cond_sim_draw_adm0, 1, quantile, probs = .025),
    upper = apply(cond_sim_draw_adm0, 1, quantile, probs = .975)
  )
  outputlist[["adm0_geo"]] <- data.table(split_geo_names(adm0_geo), adm0_geo)

  message("Calculating raking factors")
  rf <- calc_raking_factors(
    agg_geo_est = cond_sim_raw_adm0,
    rake_to = rake_to
  )

  # rake the cell preds
  message("Raking cell pred object")
  raked_cell_pred <- rake_predictions(
    raking_factors = rf,
    pop_wts_object = pop_wts_adm0,
    cell_pred = cell_pred
  )

  outputlist[["raked_cell_pred"]] <- raked_cell_pred
  outputlist[["simple_raster"]] <- simple_raster
  outputlist[["raking_factors"]] <- rf

  message("Summarizing raked cell preds")
  for (summeasure in c("mean", "cirange", "lower", "upper")) {
    message(sprintf("    %s", summeasure))
    outputlist[[sprintf("%s_raked_raster", summeasure)]] <-
      make_cell_pred_summary(
        draw_level_cell_pred = raked_cell_pred,
        mask = simple_raster,
        return_as_raster = TRUE,
        summary_stat = summeasure
      )
  }

  return(outputlist)
}
