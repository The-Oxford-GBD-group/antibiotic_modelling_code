#' @title Get weight correlation between raw data and unraked estimates
#' @description Returns the weighted correlation between the raw data points input into a model run,
#' and the unraked estimates.
#' @author Rebecca Stubbs
#'
#' @param indicator_group The string of the indicator group.
#' @param indicator The string name of the indicator.
#' @param rundate The string name of the rundate.
#'
#' @return Returns the output from a call to the weights::wtd.cors() function
#' between the raw data points and the estimates.
#' @export
get_weighted_correlation_raw_estimated <- function(indicator_group,
                                                   indicator,
                                                   rundate) {
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")
  input_folder <- paste0(root, "/WORK/11_geospatial/10_mbg/input_data/")

  message("Reading in the raw data")
  # Read in CSV, reduce table size by eliminating irrelevant columns
  raw_data <- fread(paste0(input_folder, indicator, ".csv"))
  raw_data[, raw := raw_data[[indicator]] / N]
  raw_data <- raw_data[, list(year = original_year, latitude, longitude, raw, N, weight)]

  message("Loading in the mean raster (Unraked)")
  # Load in Raster of Values
  results_tif <- raster::stack(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", rundate, "/", indicator, "_mean_raster.tif"))
  names(results_tif) <- as.character(seq(2000, 2015))

  raw_estimate_comparison <- list()

  message("Extracting values to points")
  for (yr in seq(2000, 2015)) {
    print(yr)
    raw_singleyr <- raw_data[year == yr, ]

    # Convert to spatial object
    coordinates(raw_singleyr) <- ~ longitude + latitude

    # setting the proj4string (the projection information) of this layer to be the
    # same as the results raster
    proj4string(raw_singleyr) <- proj4string(results_tif)

    # Extract raster values to points
    values <- raster::extract(results_tif[[paste0("X", as.character(yr))]], raw_singleyr)

    # Going back into table-space, and adding the data.table to a list to rbind later
    raw_singleyr <- as.data.table(raw_singleyr)
    raw_singleyr[, estimate := values]
    raw_estimate_comparison[[as.character(yr)]] <- raw_singleyr
  }

  message("Calculating correlation coefficient using the weights::wtd.cors function")
  # Combining together the raw estimates into 1 data.table
  raw_estimate_comparison <- rbindlist(raw_estimate_comparison)
  cor_results <- weights::wtd.cors(
    x = raw_estimate_comparison$raw,
    y = raw_estimate_comparison$estimate,
    weight = raw_estimate_comparison$N * raw_estimate_comparison$weight
  )

  return(cor_results)
}
