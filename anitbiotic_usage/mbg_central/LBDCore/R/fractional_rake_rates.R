#' @title Rakes a cell_pred containing rates to GBD targets
#' @description This function:
#' \enumerate{
#    \item Takes a rates cell_pred
#'   \item Links it to a fractional raking and aggregation link table
#'   \item Adds population per fractional cell
#'   \item Rakes that population to ensure matching to GBD
#'   \item Saves those raking factors
#'   \item Uses that population to create a linked counts cell_pred
#'   \item Aggregates the cell_pred to the GBD geography level
#'   \item Converts the aggregations back to rates and rakes to GBD targets
#'   \item Saves those raking fators, to a custom location if desired
#' }
#' @param cell_pred cell_pred object from mbg models.  Each cell must have a rate in it
#' @param simple_raster the simple raster that the cell_pred is based on
#' @param simple_polygon the simple polygon that the cell_pred is based on
#' @param pixel_id list of the pixels in the simple raster that have non na values
#' @param shapefile_version which shapefile geographies are being used
#' @param reg the modeling region
#' @param pop_measure the worldpop agegroup on which the model is built
#' @param year_list the modeled years
#' @param use_intermediate_years Boolean to indicate whether or not to rake to intermediate years. Default: TRUE
#' @param interval_mo the time in months between the modeled years
#' @param rake_subnational a logical value indicating the use of subnational raking targets or not
#' @param age_group the gbd age group that the model is built on
#' @param sex_id the gbd sex group that the model is built on
#' @param sharedir sharedir       <- sprintf('/share/geospatial/mbg/\%s/\%s',indicator_group,indicator)
#' @param run_date model run date
#' @param indicator modeled indicator
#' @param gbd gbd object prepared containing the raking targets
#' @param gbd_pops output from central code "get_population" function
#' @param countries_not_to_rake countries (vector or summed string) to not rake to GBD (we set rake factor to 1 for those)
#' @param countries_not_to_subnat_rake character vector of iso3 codes for countries not to subnationally rake
#' @param custom_output_folder Output the rake factors and outputs to custom folder path if specified. Default: NULL
#' @param rake_method if set to "logit" creates raking factors in logit space, otherwise assumes linear raking
#'
#' @return automatically saves out two raking factors tables, one for population and one for the indicator
#' @export
fractional_rake_rates <- function(cell_pred = cell_pred,
                                  simple_raster = simple_raster,
                                  simple_polygon = simple_polygon,
                                  pixel_id = pixel_id,
                                  shapefile_version = shapefile_version,
                                  reg = reg,
                                  pop_measure = pop_measure,
                                  year_list = year_list,
                                  use_intermediate_years = TRUE,
                                  interval_mo = interval_mo,
                                  rake_subnational = rake_subnational,
                                  age_group = age_group,
                                  sex_id = sex_id,
                                  sharedir = sharedir,
                                  run_date = run_date,
                                  indicator = indicator,
                                  gbd = gbd,
                                  rake_method = "linear",
                                  gbd_pops = gbd_pops,
                                  countries_not_to_rake = NULL,
                                  countries_not_to_subnat_rake = NULL,
                                  custom_output_folder = NULL) {
  # setting a reference for the number of draws
  ndraws <- ncol(cell_pred)

  #####################################################################
  # load the cell id to admin units link
  link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version)

  #####################################################################
  # collect and load the population data from the WorldPop rasters
  covdt <- load_populations_cov(reg, pop_measure, measure = "count", simple_polygon, simple_raster, year_list, interval_mo, pixel_id = pixel_id)

  if (!use_intermediate_years) {
    print("Subsetting to supplied years only")
    covdt <- covdt[year %in% year_list]
  }

  #####################################################################
  # Prepping the cell_pred and link table to be linked by making sure they have the appropriate identifiers.  Also performs a
  # zippering at the region boundary where cells that have a portion of their area outside of the modeling region are reintegrated
  # as a whole cell and considered to be only in one region.  This works becasue a given cell is only modeled in one region.
  link <- prep_link_table(
    link_table = link_table,
    simple_raster = simple_raster,
    pixel_id = pixel_id
  )

  cell_ids <- link_table[[2]]

  # getting the connector for sub-national or national raking, This connector gets the IHME location_code for our
  # gbd targets and connects that to the ADM0_CODE or ADM1_CODE as nessecary

  connector <- get_gbd_locs(
    rake_subnational = rake_subnational,
    reg = reg,
    shapefile_version = shapefile_version
  )

  # getting the connector for sub-national raking - used to implement countries_not_to_subnat_rake
  nat_connector <- get_gbd_locs(
    rake_subnational = F,
    reg = reg,
    shapefile_version = shapefile_version
  )

  # merge the connectors on to the link table
  link <- sub_nat_link_merge(
    rake_subnational,
    link,
    connector,
    nat_connector,
    countries_not_to_subnat_rake
  )

  # set cell pred as a data table, and rename things
  cell_pred <- prep_cell_pred(
    cell_pred = cell_pred,
    cell_ids = cell_ids,
    pixel_id = pixel_id,
    covdt = covdt
  )

  # merge cell_pred on the link
  cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)

  # space
  link <- NULL

  ## Raking Population ###################################################################
  # This is done to ensure that the total pop in each raking geography is the same as GBD
  message("raking population")

  # convert to fractional population
  cell_pred <- cell_pred[, pop := pop * area_fraction]

  # NA out population where the pixel value is NA (to prevent weirdness with denominators)
  cell_pred <- cell_pred[is.na(V1), pop := NA]

  scalars <- calculate_pop_scalars(
    cell_pred = cell_pred,
    age_group = age_group,
    connector = connector,
    sex = sex_id,
    sharedir = sharedir,
    run_date = run_date,
    indicator = indicator,
    stratum = reg,
    gbd_pops = gbd_pops,
    custom_output_folder = custom_output_folder
  )
  # add back to the cell_pred as a population rf
  cell_pred <- merge(cell_pred, scalars, by = c("location_id", "year"))

  # rake fractional populations
  cell_pred$pop_raked <- 0
  cell_pred <- cell_pred[, pop_raked := pop * pop_scalar]
  cell_pred$pop <- NULL

  ## Raking actual data ###################################################################
  message("raking rates")
  # Calculate Fractional Raking Factors
  fractional_rf <- calculate_fractional_rfs(
    ndraws = ndraws,
    cell_pred = cell_pred,
    gbd = gbd,
    sharedir = sharedir,
    run_date = run_date,
    indicator = indicator,
    shapefile_version = shapefile_version,
    stratum = reg,
    countries_not_to_rake = countries_not_to_rake,
    custom_output_folder = custom_output_folder,
    countries_not_to_subnat_rake = countries_not_to_subnat_rake,
    rake_method = rake_method
  )
}
