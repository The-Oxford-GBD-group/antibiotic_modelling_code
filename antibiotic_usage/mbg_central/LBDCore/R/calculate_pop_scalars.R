#' @title Calculate population scalars
#' @description This calculates the population raking factors needed so that prevalence rates and counts will align with GBD
#'
#' @param cell_pred the cell pred object that has been prepped and linked
#' @param age_group the GBD age_group you are modeling
#' @param connector the list of raking geographies
#' @param sex the GBD sex_id you are modeling
#' @param sharedir the share directory for the indicator group, used to save the population raking factors
#' @param run_date your run date
#' @param indicator the modeled indicator
#' @param stratum  the region you are modeling over, used for saving the population raking factors
#' @param gbd_pops output from central code "get_population" function, passed from main function
#' @param custom_output_folder Output the rake factors to custom folder path if specified. Default: NULL
#'
#' @return a table of population raking facotrs for each of the raking geographies used.  This is so that
#' the fractionally aggregated worldpop rasters for a given age and sex group in a geography year will equal
#' the GBD population for that age and sex group in that geography in that year.
#'
#' @export
calculate_pop_scalars <- function(cell_pred = cell_pred,
                                  age_group = age_group,
                                  connector = connector,
                                  sex = sex_id,
                                  sharedir = sharedir,
                                  run_date = run_date,
                                  indicator = indicator,
                                  stratum = stratum,
                                  gbd_pops = gbd_pops,
                                  custom_output_folder = NULL) {
  
  # do calculations!
  rake_geo_pop <- cell_pred[, lapply(c("pop"), function(x) sum(get(x), na.rm = T)), by = c("year", "location_id")]
  rake_geo_pop$world_pop_unraked <- rake_geo_pop$V1
  loc_ids <- unique(connector$location_id)
  
  # adjust to be GBD populations
  scalars <- merge(rake_geo_pop, gbd_pops, by.x = c("location_id", "year"), by.y = c("location_id", "year_id"))
  scalars[, pop_scalar := population / world_pop_unraked]
  
  # for records
  scalars$gbd_pop <- scalars$population
  scalars$population <- NULL
  
  # trim Scalars object
  scalars$age_group_id <- NULL
  scalars$sex_id <- NULL
  scalars$run_id <- NULL
  scalars$V1 <- NULL

  # Fill NAs with 1 BUT WITH A SOLEMN WARNING
  if(nrow(scalars[is.na(pop_scalar)]) >= 1 ) {
    warning("You have NAs in your population raking factor. Please check your GBD populations to make sure you didn't miss any locations.")
    warning("Forcing those missing raking factors to 1")
    scalars[is.na(pop_scalar), pop_scalar:= 1]
  }
  
  if (!is.null(custom_output_folder)) {
    write.csv(scalars, file = paste0(custom_output_folder, "/", indicator, "_", stratum, "_pop_rf.csv"))
  } else {
    write.csv(scalars, file = paste0(sharedir, "/output/", run_date, "/", indicator, "_", stratum, "_pop_rf.csv"))
  }
  
  return(scalars)
}
