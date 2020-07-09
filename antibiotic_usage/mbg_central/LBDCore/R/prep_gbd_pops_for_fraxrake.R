#' @title Get GBD population for use in fractional raking
#' @description Get GBD population for use in fractional raking
#'
#' @param pop_measure WorldPop measure, default: "a0004t"
#' @param reg Region
#' @param year_list the years to get population for
#'
#' @return A data.table with \code{c('location_id', 'year_id', 'sex_id', 'run_id', 'age_group_id', 'population')}
#' @note the \code{age_group_id} returned is equal to the value of \code{pop_measure}
#'
#' @export
prep_gbd_pops_for_fraxrake <- function(pop_measure = "a0004t", reg, year_list, ...) {
  if (class(year_list) == "character") year_list <- eval(parse(text = year_list))

  ## Get age group ID from pop_measure
  age_GBD <- get_age_group_from_worldpop(pop_measure)

  ## Get location ID from GBD
  loc_GBD <- unique(c(get_gbd_locs(reg)$location_id, get_gbd_locs(reg, rake_subnational = FALSE)$location_id))

  ## We need to use GBD's get_populations, NOT the LBD one, because of deprecated DB stuff
  source(paste0(CC_ENV_DIR, "/get_population.R"))
  gbd_pops <- get_population(age_group_id = age_GBD, location_id = loc_GBD, sex_id = 3, year_id = year_list, ...)

  ## Reduce to a single age group:
  gbd_pops <- gbd_pops[, .(population = sum(population)), by = c("location_id", "year_id", "sex_id", "run_id")]
  gbd_pops[, age_group_id := pop_measure]

  return(gbd_pops)
}
