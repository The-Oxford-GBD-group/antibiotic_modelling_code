#' @title Calculate raking factors
#' @description A function to logit rake MBG results
#'
#' @param cell_pred matrix. draws from an mbg model in the cell pred form
#' @param rake_targets data.table with at least 3 columns (specified by lyv) with gaul code, year and val to rake towards
#' @param lyv character vector of length 3. Denotes the gaul_code column, year column and value column-- in that order
#' @param year_list numeric vector. Vector of years implied by the cell pred.
#' @param simple_raster raster. Should be the rasterized version of a shapefile or otherwise denoting the gaul/admin codes specified in rake targets
#' @param weight_brick rasterbrick. Raster brick where the values are the spatial weightings (usually population).
#' @param rake_method character. One of 'linear' or 'logit'. Possibly more in the future
#' @param if_no_gbd default `return_na`, other option `return_unraked`. If return_na, any location-years without gbd estimates will return NA raking factors. if return_unraked, will any location-years without gbd estimates will return 1 for linear raking, 0 for logit raking.
#' @param MaxJump default `10`. Maximum size of a jump to the answer for logit raking.
#' @param MaxIter default `80`. Number of jumps towards the solution
#' @param FunTol default `1e-5`. Maximum allowed difference between the raking target and raked results
#' @param iterate default `F`. If logit raking for a location-year fails, try again with `MaxJump` and `MaxIter` times 10. If that fails, try again times 100. For circumstances where raking target is very far from estimate and raking does not converge.
#' @param zero_heuristic default `F`.  If logit raking, this will automatically set rf = -9999 for any country-year with a target value of 0.  This produces a raked value of 0 for all pixels.  Raking to a target of zero in logit space is very time-consuming and the algorithm
#'                       can only approach zero asymptotically.  For situations where the target is truly zero (most useful for, say, an intervention pre-introduction) this will both speed up the process and ensure that zeros are returned.
#' @param approx_0_1 default `F`. If logit raking, any values of zero will be replaced with 1e-10 and values of 1 will be replaced with (1-(1e-10)).  Otherwise, logit transformations will fail in `NewFindK`. Useful if some areas have very low or high predicted values in `cell_pred`,
#'                   such that some draws are either 0 or 1
#'
#' @usage a = calculate_raking_factors(cell_pred, aaa[[1]], year_list = 2000:2015, simple_raster = simple_raster, weight_brick = pop_raster_annual, rake_method = 'linear')
#' @usage b = calculate_raking_factors(cell_pred, aaa[[1]], year_list = 2000:2015, simple_raster = simple_raster, weight_brick = pop_raster_annual, rake_method = 'logit')
#' @return a data.table with raking factors
#' @export
calculate_raking_factors <- function(cell_pred,
                                     rake_targets,
                                     lyv = c("name", "year", "mean"),
                                     year_list,
                                     simple_raster,
                                     weight_brick,
                                     rake_method = c("logit", "linear"),
                                     if_no_gbd = "return_na",
                                     MaxJump = 10,
                                     MaxIter = 80,
                                     FunTol = 1e-5,
                                     iterate = F,
                                     zero_heuristic = F,
                                     approx_0_1 = F) {
  # check to make sure rake targets is a data table
  setDT(rake_targets)

  # check to make sure rake targets has valid columns
  if (!all(lyv %in% names(rake_targets))) {
    stop("rake_targets does not contain all the columns specified by lyv")
  }

  # scoping
  rake_targets <- copy(rake_targets[, lyv, with = F])
  setnames(rake_targets, lyv, c("loc", "year", "target"))

  # check to make sure all country years are represented
  sr_locs <- unique(simple_raster[])
  sr_locs <- sr_locs[!is.na(sr_locs)]
  cys <- setDT(expand.grid(loc = sr_locs, year = year_list))
  start_row <- nrow(cys)
  cys <- merge(cys, rake_targets, by = c("loc", "year"), all.x = T)

  # print missing GBD targets
  if (any(is.na(cys$target))) {
    missing_targets <- cys[is.na(target), ]
    for (i in 1:nrow(missing_targets)) {
      message("no GBD raking targets for ihme_loc_id - ", missing_targets$loc[i], ", year - ", missing_targets$year[i])
    }
  }

  # check to make sure weight_brick has the same number of years as year list
  if (dim(weight_brick)[3] != length(year_list)) {
    stop("year_list implies a different number of time steps than the weight brick")
  }

  # check to make sure simple raster, weight_brick, and cell pred all have the proper dimensions
  stopifnot(dim(simple_raster)[1:2] == dim(weight_brick)[1:2])

  # check to make sure cell pred is an accurate subset of simple raster and weight brick
  if (!dim(cell_pred)[1] / length(cellIdx(simple_raster)) == dim(weight_brick)[3]) {
    stop("cell_pred and simple_raster dimensions are not aligned")
  }

  # match rake_method
  rake_method <- match.arg(rake_method)

  ## assuming checks pass, calculate a data table from which raking factors can be calculated
  sri <- cellIdx(simple_raster)
  ngoodpixels <- length(sri)

  # figure out which pixels over time have good values
  sri_years <- unlist(lapply(seq(year_list) - 1, function(x) sri + (x * ncell(simple_raster))))
  nyears <- length(year_list)

  # first format the weight_brick
  raker <- data.table(
    pixel_means = rowMeans(cell_pred),
    pixel_xyt_id = sri_years,
    pixel_xy_id = sri,
    loc = rep.int(simple_raster[sri], nyears),
    year = as.vector(outer(Y = year_list, X = rep.int(1, ngoodpixels))),
    weight = weight_brick[][sri_years]
  )
  raker[, cell_pred_id := .I]

  # specify by_vars
  byvars <- c("loc", "year")

  # remove NA weight values from raker
  pre <- dim(raker)[1]
  raker <- raker[!is.na(weight) & !is.na(pixel_means), ]
  post <- dim(raker)[1]

  if (pre != post) {
    warning(paste0(pre - post, " (", scales::percent((pre - post) / pre), ") pixels (over the whole cube) were removed because of NA weight_brick or pixel values"))
  }

  if (rake_method == "linear") {
    # collapse by country year to get the linear adjustment
    rak <- raker[, list(px_mean = sum(pixel_means * weight, na.rm = T), sumweight = sum(weight, na.rm = T)), by = byvars]
    rak[, start_point := px_mean / sumweight]
    # merge on the target
    rak <- merge(rak, as.data.frame(rake_targets), all.x = T)
    rak[, raking_factor := target / start_point]

    if (if_no_gbd == "return_unraked") {
      rak[is.na(raking_factor), raking_factor := 1]
    }
  } else if (rake_method == "logit") {

    # for each country year, find the logit raking factor
    # redefine cys
    cys <- unique(raker[, .(loc, year)])
    rf <- lapply(seq(nrow(cys)), function(x) {

      # year and location
      theloc <- cys[x, loc]
      theyear <- cys[x, year]

      if (nrow(rake_targets[loc == theloc & year == theyear]) == 0) {
        if (if_no_gbd == "return_na") {
          return(NA)
        } else {
          return(0)
        }
      } else if (rake_targets[loc == theloc & year == theyear, .(target)] == 0 & zero_heuristic == T) {
        # catch true zeroes (i.e. pre-introduction of an intervention) and return -9999. This will speed things up & will replace later with 0
        return(-9999)
      } else {
        ret <- try(LogitFindK(
          gbdval = rake_targets[loc == theloc & year == theyear, .(target)],
          pixelval = cell_pred[raker[loc == theloc & year == theyear, cell_pred_id], ], # pass the cell pred rows that corrospond to this country year
          weightval = raker[loc == theloc & year == theyear, weight],
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
              gbdval = rake_targets[loc == theloc & year == theyear, .(target)],
              pixelval = cell_pred[raker[loc == theloc & year == theyear, cell_pred_id], ], # pass the cell pred rows that corrospond to this country year
              weightval = raker[loc == theloc & year == theyear, weight],
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
              gbdval = rake_targets[loc == theloc & year == theyear, .(target)],
              pixelval = cell_pred[raker[loc == theloc & year == theyear, cell_pred_id], ], # pass the cell pred rows that corrospond to this country year
              weightval = raker[loc == theloc & year == theyear, weight],
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

    cys[, raking_factor := unlist(rf)]
    cys <- merge(cys, rake_targets, all.x = T, by = c("loc", "year"))

    # calculate the starting point post hoc for standardized reporting
    rak <- raker[, list(px_mean = sum(pixel_means * weight, na.rm = T), sumweight = sum(weight, na.rm = T)), by = byvars]
    rak[, start_point := px_mean / sumweight]

    rak <- merge(rak, cys, all.x = T, by = c("loc", "year"))
  }

  # raking factors at the cys level
  rak <- rak[, .(loc, year, start_point, target, raking_factor)]

  return(rak)
}
