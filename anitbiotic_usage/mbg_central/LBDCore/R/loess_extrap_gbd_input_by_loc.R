#' @title Extrapolate GBD measure to future
#' @description Use a simple loess model to extrapolate GBD input data to desired year
#' 
#' @param gbd The data.table with name, year, and mean values (output from \code{get_gbd_estimates})
#' @param year_list List of in sample years (config argument)
#' @param year_forecast_end Year to forecast to. Default: 2030
#'
#' @return A data.table with name, year and mean columns extrapolated to \code{year_forecast_end}
#' 
#' @importFrom stats loess
#' @export
loess_extrap_gbd_input_by_loc <- function(gbd, year_list, year_forecast_end = 2030) {
  gbd_forecast <- data.table(expand.grid(name = unique(gbd$name), 
                                         year = union(year_list, c(max(year_list):year_forecast_end))))
  gbd_forecast <- merge(gbd_forecast, gbd, c('name', 'year'), all.x = TRUE)
  
  ## Loop over all locations
  lapply(unique(gbd$name), function(loc) {
    
    ## Run a simple loess and extrapolate in log space
    gbd_fc_vector <- predict(stats::loess(formula = log(mean) ~ year, 
                                          data = gbd_forecast[name == loc], 
                                          control = loess.control(surface = "direct")), 
                             newdata = gbd_forecast[name == loc]) 
    gbd_forecast[name == loc, mean_forecast:= gbd_fc_vector]
    
    ## Intercept shift at last year of in-sample data
    gbd_forecast[year == max(year_list), int_shift:= log(mean) - mean_forecast]
    gbd_forecast[name == loc, int_shift:= mean(int_shift, na.rm = TRUE), by = 'name']
    gbd_forecast[name == loc & year >= max(year_list), mean:= exp(mean_forecast + int_shift)]
    gbd_forecast[, c('mean_forecast', 'int_shift'):= NULL]
    return(0)
  })
  
  return(gbd_forecast)
}
