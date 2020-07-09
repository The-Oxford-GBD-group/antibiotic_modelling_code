
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param simple PARAM_DESCRIPTION, Default: NULL
#' @param agebin PARAM_DESCRIPTION, Default: 0
#' @param removeyemen PARAM_DESCRIPTION, Default: FALSE
#' @param pathaddin PARAM_DESCRIPTION, Default: ''
#' @param withdate PARAM_DESCRIPTION, Default: FALSE
#' @param date PARAM_DESCRIPTION, Default: ''
#' @param years PARAM_DESCRIPTION, Default: 'five_year'
#' @param range PARAM_DESCRIPTION, Default: 5
#' @param update_run_date PARAM_DESCRIPTION, Default: FALSE
#' @param withtag PARAM_DESCRIPTION, Default: FALSE
#' @param datatag PARAM_DESCRIPTION, Default: ''
#' @param use_share PARAM_DESCRIPTION, Default: FALSE
#' @param yl PARAM_DESCRIPTION, Default: year_list
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname load_input_data
#' @export
#' @importFrom stringr str_match
load_input_data <- function(indicator,
                            simple = NULL,
                            agebin = 0,
                            removeyemen = FALSE,
                            pathaddin = "",
                            withdate = FALSE,
                            date = "",
                            years = "five_year",
                            range = 5,
                            update_run_date = FALSE,
                            withtag = FALSE,
                            datatag = "",
                            use_share = FALSE,
                            yl = year_list) {

  ## Load input data from required location
  #   Arguments:
  #     indicator = Specific outcome to be modeled within indicator category, i.e. "edu_0"
  #     simple    = Single polygon that defines boundaries of the entire area you want to model over.
  #   Returns: Input data subset to modeling area.

  # Ensure str_match loaded
  str_match <- stringr::str_match

  if (withdate) {
    if (date == "") {
      rd <- run_date
    }
    if (date != "") {
      rd <- date
    }
  } else {
    rd <- run_date
  }

  # Load input data by indicator
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")
  if (use_share == FALSE) load_dir <- paste0(root, "/WORK/11_geospatial/10_mbg/input_data/")
  if (use_share == TRUE) load_dir <- "/share/geospatial/mbg/input_data/"

  if (!withdate & !withtag) filename <- paste0(load_dir, indicator)
  if (withtag) filename <- paste0(load_dir, indicator, datatag)
  if (withdate) filename <- paste0(root, "/WORK/11_geospatial/10_mbg/input_data/dated/", rd, "/", indicator)

  # try to see if an RDS exists, if so use that, if not use a csv
  if (file.exists(paste0(filename, ".RDS"))) {
    message("READING INPUT DATA FROM RDS FILE")
    d <- readRDS(paste0(filename, ".RDS"))
  } else {
    message("READING INPUT DATA FROM CSV FILE")
    d <- read.csv(paste0(filename, ".csv"))
  }

  d$latitude <- as.numeric(as.character(d$latitude))
  d$longitude <- as.numeric(as.character(d$longitude))
  message(nrow(d))
  d <- d[d$latitude <= 90, ]
  d <- d[d$latitude >= -90, ]
  d <- d[d$longitude <= 180, ]
  d <- d[d$longitude >= -180, ]
  d <- subset(d, !is.na(latitude))
  d <- subset(d, latitude != 0)
  message(nrow(d))

  # Check for necessary columns
  if (!(indicator %in% names(d))) stop(paste0("Your input data does not contain a column for your indicator: ", indicator))

  # Subset to within modeling area
  if (!is.null(simple)) {
    coordinates(d) <- c("longitude", "latitude")
    proj4string(d) <- proj4string(simple)
    d$keep <- !is.na(over(d, as(simple, "SpatialPolygons")))
    message(paste0(round(mean(d$keep), 2) * 100, "% of input data in specified template"))
    d <- d[d$keep == TRUE, ]
  }
  d <- as.data.table(d)

  if (agebin != 0) d <- d[age %in% agebin, ]
  if (removeyemen) d <- d[country != "Yemen" & country != "YEM", ]

  # remap any years as needed
  if (years == "five_year") {
    d <- d[year >= 1998 & year <= 2002, year := 2000]
    d <- d[year >= 2003 & year <= 2007, year := 2005]
    d <- d[year >= 2008 & year <= 2012, year := 2010]
    d <- d[year >= 2013 & year <= 2017, year := 2015]
  }

  if (nrow(subset(d, year < min(yl))) > 0) {
    warning(paste0("Dropping all data before min(year_list) = ", min(yl), "..."))
    d <- subset(d, year >= min(yl))
  }
  if (nrow(subset(d, year > max(yl))) > 0) {
    warning(paste0("Dropping all data after max(year_list) = ", max(yl), "..."))
    d <- subset(d, year <= max(yl))
  }

  # Change all "country" assignments to national level (in case subnational in the input data)
  if (nrow(d[grepl("[A-Z]*_[.]*", country), ]) > 0) {
    subnat_countries <- unique(d[grepl("[A-Z]*_[.]*", country), country])
    warning(paste0(
      "Changing subnational to national country codes for the following: ",
      paste0(subnat_countries, collapse = ",")
    ))
    d[grepl("[A-Z]*_[.]*", country), country := str_match(country, "([A-Z]*)_[.]*")[, 2]]
  }

  # creaste a weighted SS to base QTs on
  if (sum(c("N", "weight") %in% colnames(d)) == 2) d[, weighted_n := N * weight]

  # Save a copy
  if (update_run_date == TRUE) {
    if (dir.exists(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)) == TRUE) {
      existing_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
      new_try <- existing_dir
      index <- 0
      while (dir.exists(new_try)) {
        index <- index + 1
        new_try <- paste0(existing_dir, "_", index)
      }
      run_date <- paste0(run_date, "_", index)
      dir.create(new_try, showWarnings = FALSE)
      run_date_dir <- new_try
    }
    if (dir.exists(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)) == FALSE) {
      run_date_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
      dir.create(run_date_dir, showWarnings = FALSE)
    }
    write.csv(d, file = paste0(run_date_dir, "/input_data", pathaddin, ".csv"))
    return(list(d, run_date))
  }

  if (update_run_date == FALSE) {
    if (agebin == 0) {
      run_date_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
      dir.create(run_date_dir, showWarnings = FALSE)
      write.csv(d, file = paste0(run_date_dir, "/input_data", pathaddin, ".csv"))
    } else {
      run_date_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "_age", agebin, "/output/", run_date)
      dir.create(run_date_dir, showWarnings = FALSE)
      write.csv(d, file = paste0(run_date_dir, "/input_data", pathaddin, ".csv"))
    }
    return(d)
  }
}
