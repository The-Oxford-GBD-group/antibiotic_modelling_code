#' @title Function to perform polygon resampling
#'
#' @description Resample polygons using the k-means clustering algorithm developed by RB et al
#'
#' @param data a data frame or data table with the following columns: cluster_id, exposed,
#'             <indicator>, latitude, longitude, shapefile, location code. Location_code refers to
#'             the shapefile polygon that the data. Data should alreaday be aggregated up
#'             to the smallest geog unit (if this is point, then lat long should have values).
#'             If lat long don't have values then shapefile and location_id should
#' @param shp_path Path to shapefiles (standard = our shapefile directory)
#' @param ignore_warnings if TRUE, and there are polys that are not found the function will warn but not stop.
#' @param cores Number of cores to use for parallel processing bits
#' @param indic Indicator being run (e.g. `died`, `dpt3_cov`, `has_lri`.  Note that this is only used for some very specific
#'              renaming at the very end of the function - this code will actually preserve all columns passed to it.
#' @param unique_vars character values of additional names that uniquely ID rows in data, for example "age_bin" for
#'                    u5m data. Defaults to NULL
#' @param density Passed to `seegMBG::getPoints()` as the `n` argument, which determines the # of integration points.
#'                Determines how many pseudopoints are laid down for each polygon. Default = 0.001.
#'                This can be interpreted as points per pixel if `perpixel = T`, and as points per square km if `perpixel = T`
#'                and `use_1k_popraster = T` (default settings)
#' @param perpixel Passed to `seegMBG::getPoints()` as the `perpixel` argument  This determines whether the `density`
#'                 arguments represents an absolute number, or if it is the number of integration points per pixel (so
#'                 that the total # of integration points = `density` * `[total number of pixels]`).  By default this is
#'                 TRUE with a density of 0.001.
#' @param prob Passed to `seegMBG::getPoints()` as the `prob` argument. Determines whether to weight the integration points
#'             by the values of the raster (here, the population raster). TRUE by default.
#' @param use_1k_popraster Should a 1km x 1km population raster be used (disaggregates the 5x5 km population raster)?
#'                         Logical, default = TRUE.  Otherwise will use 5k pop raster
#' @param pull_poly_method How should we get the polygons for resampling?  Options are parallel processing with `mclapply()`
#'                         (`pull_poly_method = "mclapply"`), parallel processing with `foreach()` (`pull_poly_method = "foreach"`)
#'                         or fast polygon pulling using RDS files (`pull_poly_method = "fast"` -- the default)
#' @param gaul_list List of gaul codes covering your modeling area, for use in pulling the population rasters
#' @param seed Optional seed that is set just prior to resampling to gaurantee the same results when rerunning on the same data.
#'             By default this is `NULL`, in which case no seed is set.
#' @param shapefile_version string indicating which admin shapefile to use to generate population raster
#'
#' @return A data table with your input `data` object, now with polygons duplicated and weighed according to the resampling
#'         algorithm
#'
#' @examples
#' \dontrun{
#' # Will not run unless you have vaccine data
#' df_pointpoly <- resample_polygons(
#'   data = df_input,
#'   cores = 20,
#'   indic = "dpt3_cov",
#'   density = 0.001
#' )
#' }
#' @export
resample_polygons <- function(data,
                              shp_path = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory",
                              ignore_warnings = TRUE,
                              cores = as.numeric(slots),
                              indic = indicator,
                              unique_vars = NULL,
                              density = 0.001,
                              perpixel = TRUE,
                              prob = TRUE,
                              use_1k_popraster = TRUE,
                              pull_poly_method = "fast",
                              gaul_list = get_adm0_codes("africa", shapefile_version = "current"),
                              seed = NULL,
                              shapefile_version = "current",
                              custom_ras = NULL,
                              years = NULL) {

  ############################
  data <- data.frame(data) # df not dt for this one

  # Confirm necessary columns in data
  nec_cols <- c("shapefile", "location_code")

  if (any(!(nec_cols %in% names(data)))) {
    message("Missing the following columns in your data:")
    print(nec_cols[!nec_cols %in% names(data)])
  }

  # change lat long to latiude longitude if needed
  names(data)[names(data) == "lat"] <- "latitude"
  names(data)[names(data) == "long"] <- "longitude"

  # find records with shapefiles
  data$shapefile[!is.na(data$latitude) & !is.na(data$longitude)] <- NA
  noloc <- rep(FALSE, nrow(data))
  noloc[data$shapefile == "" & !is.na(data$shapefile)] <- TRUE

  # remove any spatially unidentifiable data
  message(paste("Dropping", sum(noloc), "of", nrow(data), "rows of data due to no spatial identifiers (lat, long, shapefile info missing)\n"))
  data <- data[!noloc, ]

  # keep index of only polygon data
  shp_idx <- which(!is.na(data$shapefile))
  message(paste(length(shp_idx), "of", nrow(data), "rows of data are polygon assigned\n"))

  # hotfix for a weird yemen thing
  data$shapefile[data$shapefile == "matched to GADM admin 1 shapefile" & data$country == "Yemen"] <- "YEM_adm1_GADM"

  # identify all shapefiles from the dataset
  all_shapes <- unique(data$shapefile[shp_idx])
  message(paste(length(all_shapes), "unique shapefiles found in data.\n"))

  # check they're in the directory
  message(paste0("Checking shapefile directory (", shp_path, ") for matches.. "))
  if (!all(paste0(all_shapes, ".shp") %in% list.files(shp_path))) {
    message("Missing the following shapefiles:")
    print(all_shapes[!(paste0(all_shapes, ".shp") %in% list.files(shp_path))])
    if (!ignore_warnings) stop("Function breaking because not all shapefiles are a match. Please")
  } else {
    message("All shapefiles in data match a shapefile by name in the directory.\n")
  }

  ############################
  # load and extract all polygons - now in parallel

  # get unique shapefiles/ location codes
  shapes <- unique(data[, c("shapefile", "location_code")])
  shapes <- shapes[!is.na(shapes$shapefile), ]
  n_shapes <- nrow(shapes)

  # sort by shapefile name (faster if they're all clumped together)
  o <- order(shapes$shapefile)
  shapes <- shapes[o, ]

  # empty, named list to store polygons
  polys <- list()
  polys[[n_shapes]] <- NA
  names(polys) <- paste(shapes$shapefile, shapes$location_code, sep = "__")

  # null first shapefile
  shape <- ""

  # report to the user
  message(sprintf("extracting %i polygons", n_shapes))

  # vector of shapefiles
  shapefiles <- unique(shapes$shapefile)

  if (pull_poly_method == "foreach") {
    polys <- pull_polys_foreach(cores, shapefiles, shapes, shp_path)
  } else if (pull_poly_method == "mclapply") {
    polys <- pull_polys_mclapply(cores, shapefiles, shapes, shp_path)
  } else if (pull_poly_method == "fast") {
    polys <- pull_polys_fast(shapefiles, shapes, shp_path)
  } else {
    stop("pull_poly_method must be one of \"foreach\", \"mclapply\", or \"fast\"")
  }

  problem_shapes <- c()
  for (i in 1:length(polys)) {
    problem_shapes <- c(problem_shapes, polys[[i]][["problem_shapes"]])
  }
  problem_shapes <- problem_shapes[is.na(problem_shapes) == F]

  # find ones that didn't work
  if (length(problem_shapes) != 0) {
    message(sprintf("%i polygons could not be found:", length(problem_shapes), ". Dropping these by default. Please check and fix"))

    problem_shapes <- as.data.table(problem_shapes)
    drop_shapes <- separate(problem_shapes, col = "problem_shapes", sep = "__", into = c("shapefile", "location_code")) %>% as.data.table()
    drop_shapes$location_code <- as.integer(drop_shapes$location_code)
    print(drop_shapes)
    if (!ignore_warnings) {
      stop("Since ignore_warnings==FALSE, the function is now breaking. Please fix your bad records or set ignore_warnings==TRUE to drop them.\n")
    }
    message("Since you have set ignore_warnings=TRUE, the function will drop all bad records and continue. Note: this is NOT recommended.\n")
    data <- data[!(paste(data$shapefile, data$location_code, sep = "__") %in%
      problem_shapes$problem_shapes), ]
  }

  polys <- unlist(polys) # get to single-level list
  polys[grep("problem_shapes", names(polys))] <- NULL

  ######################################
  ##
  message("In parallel, generating integration points for each shapefile and expanding records to integrate. \n")

  # get unique sets of shapefiles and location codes (bins optional)
  if (is.null(custom_ras) == F) {
    d <- data[, c(unique_vars, "shapefile", "location_code", "year")]
    pars <- unique(d) %>% as.data.table()
    if (is.null(years) == F) {
      d$row_id <- c(1:nrow(d))
      pars[year > max(years), year := max(years)]
      pars[year < min(years), year := min(years)]
      year_connect <- data.table(year = years, index = c(1:length(years)))
      pars <- merge(pars, year_connect, by = "year")
      pars$year <- NULL

      d <- merge(d, year_connect, by = "year", sort = F)
      d <- d[order(d$row_id), ] # make sure d is in the same order as before. Important for pairing up dx and px to get indices later
      d$year <- NULL
      d$row_id <- NULL
    } else {
      message("You've specified a custom raster to use for resampling but have not specified a year sequence or a single year to use.
              By default, the first/only layer of specified raster will be used")
      d$year <- NULL
      d$index <- 1
      pars[, index := 1]
    }
  } else { # Default is to use the standard resampling template: 2010 population raster
    d <- data[, c(unique_vars, "shapefile", "location_code")]
    pars <- unique(d) %>% as.data.table()
    pars[, index := 3] # 2010 population raster
    pars <- pars[!is.na(pars$shapefile), ]
    d$index <- 3
  }

  # get row indices
  message("Getting indices and chunking out data by polygon in parallel.\n")
  n_chunk <- nrow(pars)
  dx <- trim(apply(d, 1, paste, collapse = "--"))
  px <- trim(apply(pars[, .(shapefile, location_code, index)], 1, paste, collapse = "--"))

  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  indices <- mclapply(1:n_chunk, function(x) {
    unname(which(dx == px[x]))
  }, mc.cores = cores)

  chunks <- mclapply(1:n_chunk, function(x) {
    data[indices[[x]], ]
  }, mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  # grab population raster
  message("Loading Population Raster.\n")
  simpp <- suppressMessages(suppressWarnings(load_simple_polygon(
    gaul_list = gaul_list, buffer = 0.4, subset_only = TRUE,
    shapefile_version = shapefile_version
  )))
  raster_list <- suppressMessages(suppressWarnings(build_simple_raster_pop(simpp$subset_shape)))
  if (use_1k_popraster) {
    popraster <- disaggregate(raster_list[["pop_raster"]], 5) # needs to be 1km for density 0.001
    # popraster=brick('/share/geospatial/pop_density/africa_1km_pop.tif')
  } else {
    popraster <- raster_list[["pop_raster"]]
  }

  # get points in parallel
  message("Running getPoints() on each polygon in parallel.\n")

  if (is.null(custom_ras) == TRUE) { # use pop raster by default for resampling
    resample_ras <- popraster
  } else {
    resample_ras <- raster::crop(custom_ras, extent(popraster[[3]]))
  }

  getPointsWrapper <- function(x) {
    poly_name <- paste(pars[x, c("shapefile", "location_code")], collapse = "__")
    poly <- polys[[poly_name]]

    id <- pars[x, index]
    # NOTE: simply resampling on 2010 population data for now for convenience. Could redo by year, but wouldnt make much of difference anyway.
    if (is.null(poly)) {
      # make a dummy, empty points dataframe
      points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
    } else {
      if (!is.null(seed)) set.seed(seed)
      points <- try(
        getPoints(
          shape = poly,
          raster = resample_ras[[id]],
          n = density,
          perpixel = perpixel,
          prob = prob
        )
      )
      if (inherits(points, "try-error")) {
        points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
      } else {
        colnames(points) <- c("longitude", "latitude", "weight")
      }
    }
    return(points)
  }
  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  chunk_points <- mclapply(1:n_chunk, getPointsWrapper, mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  # duplicate records, and add their integration points
  message("Duplicating polygon records for each of their new integration points.\n")
  getNewChunksWrapper <- function(x) {
    # data for this chunk (shapefile/age bin combo)
    chunk <- chunks[[x]]
    points <- chunk_points[[x]]

    if (nrow(points) == 0) {
      new_chunk <- chunk[0, ]
      message(paste("Chunk", x, "missing spatial info and will be dropped.", paste0(chunk$shapefile, "__", chunk$location_code)))
    } else {
      dupRecords <- function(j) {
        record <- chunk[j, , drop = FALSE] # pull out the record
        # drop columns also in "points"
        record <- record[, !(colnames(record) %in% colnames(points))]
        # faster than rbind / replicate
        record_dup <- cbind(record, points, row.names = NULL)

        return(record_dup)
      }

      duped_records <- lapply(1:nrow(chunk), dupRecords)

      # append the duplicated data
      new_chunk <- rbindlist(duped_records)
    }
    return(new_chunk)
  }

  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  new_chunks <- mclapply(1:n_chunk, getNewChunksWrapper, mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  # remove old chunks from data
  message("Finishing up..\n")
  idx_all <- unlist(indices)
  data <- data[-idx_all, ]

  # append new chunks
  new_chunks_all <- rbindlist(new_chunks, fill = TRUE)
  new_chunks_all$pseudocluster <- TRUE
  if (nrow(data) > 0) {
    data$weight <- 1
    data$pseudocluster <- FALSE
    data <- rbind(data, new_chunks_all, fill = TRUE)
  } else {
    data <- new_chunks_all
  }

  # count rows with no lat/longs and remove them
  no_ll <- which(is.na(data$latitude) | is.na(data$longitude))
  message(paste("Dropping", length(no_ll), "rows with missing spatial information."))
  if (length(no_ll) != 0) data <- data[-no_ll, ]

  # remove the shapefile and location_code columns
  # data <- data[, !(colnames(data) %in% c('shapefile', 'location_code'))]

  # last minute renames to fit broader naming convention
  data <- data.table(data)
  return(data)
}
