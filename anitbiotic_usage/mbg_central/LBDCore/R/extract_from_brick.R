#' @title Extract from brick
#'
#' @description Extract values from a raster or rasterbrick to lat/lon points by relevant year of data.
#'
#' @author Rebecca Stubbs
#'
#' @param data A data.table or data.frame (that will be converted to a data.table) with
#'             columns for latitude, longitude, and year.
#' @param yearcol A string name of a column that defines the year of the data within the data object
#' @param varname A string name of what you want the column of extracted values to be
#' @param raster_years A vector that describes the years that serve as bands in the rasterbrick, in order.
#' @param rb A rasterbrick with the bands that correspond to the raster_years.
#' @return Returns the data object, sorted based on year, with a new column with the extracted values (name
#' as specified based on the varname parameter).
#' @export
extract_from_brick <- function(data,
                               yearcol = "original_year",
                               rb,
                               varname = "extracted_values",
                               raster_years = 2000:2015) {

  # Making sure you've passed the right arguments to the function:
  if (sum(c("longitude", "latitude", yearcol) %in% names(data)) != 3) {
    stop("You need to have the columns longitude, latitude, and a specified column that describes year (as the yearcol parameter) in the data object passed to this function.")
  }
  if (varname == "extracted_values") {
    warning("The variable added to the data.table will be named 'extracted_values' unless you supply a different name to the varname parameter.")
  }

  if (min(data[[yearcol]]) < min(raster_years)) {
    stop("You have years of raw input data that extend beyond the min or max of the raster years supplied to the function. Consider setting years outside those bounds to the min/max of the raster years.")
  }

  # Ordering the data object by year
  data <- data[order(data[[yearcol]])]

  # Renaming the rasterbrick layers to be descriptive of year
  names(rb) <- paste0("year_", as.character(raster_years))

  # Define a vector to store the results
  extracted_values <- c()

  for (yr in raster_years) {
    message(paste0("Extracting values for ", yr))
    # Select only the data points from that year
    singleyr <- data[data[[yearcol]] == yr, ]

    # Convert to spatial object
    coordinates(singleyr) <- ~ longitude + latitude

    # setting the proj4string (the projection information) of this layer to be the
    # same as the results raster
    proj4string(singleyr) <- proj4string(rb)

    # Extract raster values to points
    values <- raster::extract(rb[[paste0("year_", as.character(yr))]], singleyr)
    extracted_values <- c(extracted_values, values)
  }

  data[[varname]] <- extracted_values
  return(data)
}
