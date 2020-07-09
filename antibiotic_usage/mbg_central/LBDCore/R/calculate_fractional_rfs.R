#' @title Calculate fractional raking factor
#' @description This calculates the raking factors, it takes a rates cell_pred which is the main difference between it and the
#' get_fractional_rf function above (see Michael Collison's work).
#'
#' @param ndraws the number of draws used in this model run, can be calculated from the number of columns in a raw cell_pred
#' @param cell_pred the preped and linked cell_pred that has also had its population raked.
#' @param gbd a data frame of the gbd targets that we are raking to
#' @param sharedir the share directory for the indicator group, used to save the raking factors
#' @param run_date your run date
#' @param indicator the modeled indicator
#' @param shapefile_version the shapefile version
#' @param stratum  the region you are modeling over, used for saving the raking factors
#' @param countries_not_to_rake countries (vector or summed string) to not rake to GBD (we set rake factor to 1 for those)
#' @param custom_output_folder Output the rake factors to custom folder path if specified. Default: NULL
#' @param countries_not_to_subnat_rake countries (vector or summed string) to not rake to subnationals (we set rake factor to 1 for those)
#' @param MaxJump default `10`. Maximum size of a jump to the answer (for logit raking) - this will likely never need to be changed
#' @param MaxIter default `80`. Number of jumps towards the solution (for logit raking) - this will likely never need to be changed
#' @param FunTol default `1e-5`. Maximum allowed difference between the raking target and raked results (for logit raking) - this will likely never need to be changed
#' @param iterate default `F`. If logit raking for a location-year fails, try again with `MaxJump` and `MaxIter` times 10. If that fails, try again times 100. For circumstances where raking target is very far from estimate and raking does not converge.
#' @param zero_heuristic default `F`.  If logit raking, this will automatically set rf = -9999 for any country-year with a target value of 0.  This produces a raked value of 0 for all pixels.  Raking to a target of zero in logit space is very time-consuming and the algorithm
#'                       can only approach zero asymptotically.  For situations where the target is truly zero (most useful for, say, an intervention pre-introduction) this will both speed up the process and ensure that zeros are returned.
#' @param approx_0_1 default `F`. If logit raking, any values of zero will be replaced with 1e-10 and values of 1 will be replaced with (1-(1e-10)).  Otherwise, logit transformations will fail in `NewFindK`. Useful if some areas have very low or high predicted values in `cell_pred`,
#'                   such that some draws are either 0 or 1
#' @param rake_method if set to "logit" creates raking factors in logit space, otherwise assumes linear raking
#'
#' @return a table of raking factors for each of the raking geographies used.  This is so that
#' the fractionally aggregated mbg estimates rasters for a given geography year will equal
#' the GBD estimates for that geography in that year.  This also assumes that the GBD is internally consistent
#' in converting rates to counts, namely rate*pop=count
#'
#' @export

calculate_fractional_rfs <- function(ndraws = ndraws,
                                     cell_pred = cell_pred,
                                     gbd = gbd,
                                     sharedir = sharedir,
                                     run_date = run_date,
                                     indicator = indicator,
                                     shapefile_version = shapefile_version,
                                     stratum = stratum,
                                     countries_not_to_rake = NULL,
                                     custom_output_folder = NULL,
                                     countries_not_to_subnat_rake = NULL,
                                     MaxJump = 10,
                                     MaxIter = 80,
                                     FunTol = 1e-5,
                                     approx_0_1 = F,
                                     zero_heuristic = F,
                                     iterate = F,
                                     rake_method = "linear") {
  message("converting from prevalence to counts")
  # set the variables to aggregate
  overs <- paste0("V", 1:ndraws)

  # convert to counts
  cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) * pop_raked)]

  # do calculations!
  rake_geo <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "location_id")]
  setnames(rake_geo, grep("V[0-9]", names(rake_geo), value = T), c(overs, "pop_raked"))

  # convert back to rates/prevalence
  rake_geo <- rake_geo[, (overs) := lapply(overs, function(x) get(x) / pop_raked) ]

  # merge to admin estimates
  rake_geo <- merge(rake_geo, gbd, by.x = c("location_id", "year"), by.y = c("name", "year"), all.x = TRUE)

  # finding the mean of the draws at the raking geographies
  rake_geo$gbd_prev <- rake_geo$mean
  rake_geo[, mbg_rate := rowMeans(.SD), .SDcols = paste0("V", c(1:ndraws))]
  rake_geo$rf <- rake_geo$gbd_prev / rake_geo$mbg_rate

  # Clear Out
  message("creating fractional raking factors table")
  fractional_rf <- rake_geo
  fractional_rf$mbg_prev <- fractional_rf$mbg_rate
  fractional_rf <- fractional_rf[, c("location_id", "year", "mbg_prev", "gbd_prev", "rf")]


  if (rake_method == "logit") {
    cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) / pop_raked)]
    # for each country year, find the logit raking factor
    # redefine cys
    cys <- unique(rake_geo[, .(location_id, year)])
    rf <- lapply(seq(nrow(cys)), function(x) {

      # year and location
      theloc <- cys[x, location_id]
      theyear <- cys[x, year]

      if (nrow(rake_geo[location_id == theloc & year == theyear]) == 0) {
        return(0)
      } else if (rake_geo[location_id == theloc & year == theyear, .(mean)] == 0 & zero_heuristic == T) {
        # catch true zeroes (i.e. pre-introduction of an intervention) and return -9999. This will speed things up & will replace later with 0
        return(-9999)
      } else {
        ret <- try(LogitFindK(
          gbdval = rake_geo[location_id == theloc & year == theyear, mean],
          pixelval = as.matrix(cell_pred[location_id == theloc & year == theyear & !is.na(V1), paste0("V", 1:ndraws), with = F]), # pass the cell pred rows that corrospond to this country year
          weightval = as.numeric(cell_pred[location_id == theloc & year == theyear & !is.na(V1), pop_raked]),
          MaxJump = MaxJump,
          MaxIter = MaxIter,
          FunTol = FunTol,
          approx_0_1 = approx_0_1
        ))


        if (iterate) {
          # Iterate over larger values of MaxJump and NumIter if needed
          if (is.null(ret) | "try-error" %in% class(ret)) {
            message(paste0("Your GBD and MBG estimates are quite far apart for location ", theloc, " | year ", theyear))
            message("Increasing MaxJump and NumIter by 10-fold, but you should investigate this...")

            ret <- try(LogitFindK(
              gbdval = rake_geo[location_id == theloc & year == theyear, mean],
              pixelval = as.matrix(cell_pred[location_id == theloc & year == theyear & !is.na(V1), paste0("V", 1:ndraws), with = F]), # pass the cell pred rows that corrospond to this country year
              weightval = as.numeric(cell_pred[location_id == theloc & year == theyear & !is.na(V1), pop_raked]),
              MaxJump = MaxJump * 10,
              MaxIter = MaxIter * 10,
              FunTol = FunTol,
              approx_0_1 = approx_0_1
            ))
          }

          # If we get this far, the estimates are generally *really* far apart
          if (is.null(ret) | "try-error" %in% class(ret)) {
            message(paste0("Your GBD and MBG estimates are REALLY far apart for location ", theloc, " | year ", theyear))
            message("Increasing MaxJump and NumIter by 100-fold, but you should investigate this...")

            ret <- try(LogitFindK(
              gbdval = rake_geo[location_id == theloc & year == theyear, mean],
              pixelval = as.matrix(cell_pred[location_id == theloc & year == theyear & !is.na(V1), paste0("V", 1:ndraws), with = F]), # pass the cell pred rows that corrospond to this country year
              weightval = as.numeric(cell_pred[location_id == theloc & year == theyear & !is.na(V1), pop_raked]),
              MaxJump = MaxJump * 100,
              MaxIter = MaxIter * 100,
              FunTol = FunTol,
              approx_0_1 = approx_0_1
            ))
          }

          # Throw error if previous two didn't work
          if (is.null(ret) | "try-error" %in% class(ret)) {
            stop(paste0("Your GBD and MBG estimates are WAY TOO far apart for location ", theloc, " | year ", theyear, " - stopping."))
          }
        }

        return(ret)
      }
    })
    rf <- unlist(rf)
    fractional_rf$rf <- rf
  }

  # Don't rake if countries_not_to_rake is provided
  if (!is.null(countries_not_to_rake)) {
    
    # Get the GBD location IDs from the ISO codes to set raking
    # factors to 1. 
    nonrake_table <- get_gbd_locs(
      reg = countries_not_to_rake,
      rake_subnational = FALSE, shapefile_version = shapefile_version
    )
    if (nrow(nonrake_table) > 0) {
      fractional_rf[location_id %in% nonrake_table$location_id, rf := 1]
    }
    rm(nonrake_table)
  }

  # Don't subnational rake if countries_not_to_subnat_rake is provided
  if (!is.null(countries_not_to_subnat_rake)) {
    
    # Get the GBD location IDs from the ISO codes to set raking
    # factors to 1. 
    # The main difference here from the above case is that we are pulling
    # in the subnational location IDs if they exist, and also filtering for the
    # case where rak_level (raking level) is >=1 (anything more detailed than ADM0)
    nonrake_table <- get_gbd_locs(
      reg = countries_not_to_subnat_rake,
      rake_subnational = TRUE,
      shapefile_version = shapefile_version
    )[rak_level >= 1]
    if (nrow(nonrake_table) > 0) {
      fractional_rf[location_id %in% get_gbd_locs(
        reg = countries_not_to_subnat_rake,
        rake_subnational = TRUE, shapefile_version = shapefile_version
      )$location_id, rf := 1]
    }
    rm(nonrake_table)
  }

  # Fill NAs with 1 BUT WITH A SOLEMN WARNING
  if (nrow(fractional_rf[is.na(rf)]) >= 1) {
    warning("You have NAs in your output raking factor. Please check your GBD outputs to make sure you didn't miss any locations.")
    warning("Forcing those missing raking factors to 1")
    fractional_rf[is.na(rf), rf := 1]
  }

  # saving the raking factors
  if (!is.null(custom_output_folder)) {
    write.csv(fractional_rf, file = paste0(custom_output_folder, "/", indicator, "_", stratum, "_rf.csv"))
  } else {
    write.csv(fractional_rf, file = paste0(sharedir, "/output/", run_date, "/", indicator, "_", stratum, "_rf.csv"))
  }

  return(fractional_rf)
}
