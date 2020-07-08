#' @title Load worldpop covariate raster and return
#' @description Loads a covariate raster (worldpop by default, can be overridden) and returns.
#' @param template_raster the raster template which all returned data will match.
#' @param covariate the covariate to load. Defaults to "worldpop"
#' @param pop_measure the covariate measure to load.
#' @param pop_release the covariate measure release to use.
#' @param start_year the first year to locate data for. Defaults the minimum value in the configuration value \code{year_list}
#' @param end_year the last year to locate data for. defaults the maximum value in the configuration value \code{year_list}
#' @param interval the number of months between data readings. Defaults to the global interval_mo
#' @examples
#' \dontrun{
#' worldpop <- load_worldpop_covariate(simple_polygon, measure = 'a0004t', release = '2019_06_10')
#'
#' raked_worldpop <- load_worldpop_covariate(simple_polygon,
#'                                           covariate = 'worldpop_raked',
#'                                           measure = 'a0004t',
#'                                           release = '2019_06_10',
#'                                           start_year = 2000,
#'                                           end_year = 2017,
#'                                           interval = 12)
#' }
#' @export
#' @rdname load_worldpop_covariate
#' @return list with named value containing your covariate data as a raster.
load_worldpop_covariate <- function(template_raster,
                                    covariate = "worldpop",
                                    pop_measure,
                                    pop_release,
                                    start_year = min(year_list),
                                    end_year = max(year_list),
                                    interval = interval_mo) {
  worldpop_config <- data.table(covariate = c(covariate),
                                measure = c(pop_measure),
                                release = c(pop_release))

  loader <- MbgStandardCovariateLoader$new(start_year = start_year,
                                           end_year = end_year,
                                           interval = interval,
                                           covariate_config = worldpop_config)
  return(loader$get_covariates(template_raster))
}
