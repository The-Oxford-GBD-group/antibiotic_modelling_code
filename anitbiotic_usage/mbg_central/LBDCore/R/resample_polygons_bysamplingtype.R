#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param shp_path PARAM_DESCRIPTION, Default: '/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory'
#' @param ignore_warnings PARAM_DESCRIPTION, Default: TRUE
#' @param cores PARAM_DESCRIPTION, Default: as.numeric(slots)
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param unique_vars PARAM_DESCRIPTION, Default: NULL
#' @param density PARAM_DESCRIPTION, Default: 0.001
#' @param perpixel PARAM_DESCRIPTION, Default: TRUE
#' @param prob PARAM_DESCRIPTION, Default: TRUE
#' @param use_1k_popraster PARAM_DESCRIPTION, Default: TRUE
#' @param raster_dict PARAM_DESCRIPTION, Default: NULL
#' @param raster_col PARAM_DESCRIPTION, Default: NULL
#' @param regions PARAM_DESCRIPTION, Default: NULL
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname resample_polygons_bysamplingtype
#' @export
resample_polygons_bysamplingtype <- function(data,
                                             shp_path = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory",
                                             ignore_warnings = TRUE,
                                             cores = as.numeric(slots),
                                             indic = indicator,
                                             unique_vars = NULL,
                                             density = 0.001,
                                             perpixel = TRUE,
                                             prob = TRUE,
                                             use_1k_popraster = TRUE,
                                             raster_dict = NULL,
                                             raster_col = NULL,
                                             regions = NULL,
                                             shapefile_version = "current") {


  #####################################################################
  ## Preserving for backwards compatability, although we should wrap
  ## this functionality into the main function at some point if still
  ## being used...
  #####################################################################


  ## ##########################
  data <- data.frame(data) # df not dt for this one

  ## make sure things are ok if using raster_dict and raster_col
  if (!is.null(raster_dict)) {
    if (is.null(raster_col)) {
      stop("If using raster_dict must identify raster_col")
    }

    ## check all entries in raster col match entries in raster_dict
  }

  # Confirm necessary columns in data ------------------------------------------- may need further review ers------------
  nec_cols <- c("sampling", "shapefile", "location_code", "N", "cluster_id")

  if (any(!(nec_cols %in% names(data)))) {
    message("Missing the following columns in your data:")
    print(nec_cols[!nec_cols %in% names(data)])
  } else {
    message("All necessary fields in your data! Your future is bright.")
  }

  # change lat long to latiude longitude if needed
  # data = rename(data,c('lat'='latitude','long'='longitude'))
  names(data)[names(data) == "lat"] <- "latitude"
  names(data)[names(data) == "long"] <- "longitude"


  # find records with shapefiles
  data$shapefile[!is.na(data$latitude) & !is.na(data$longitude)] <- NA # this was using lat and long ers
  noloc <- rep(FALSE, nrow(data))
  noloc[data$shapefile == "" & !is.na(data$shapefile)] <- TRUE
  noloc[is.na(data$latitude) & is.na(data$longitude) & is.na(data$shapefile)] <- TRUE

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
  message("Extracting all polygons from shapefiles on disk -- in parallel.\n")
  message(sprintf("extracting %i polygons", n_shapes))

  # set up cluster foreach-style
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  # distribute lib path and packages
  clusterCall(cl, function(x) .libPaths(x), .libPaths())

  clusterCall(cl, function(x) {
    library(raster)
    library(magrittr)
  })

  # vector of shapefiles
  shapefiles <- unique(shapes$shapefile)

  # run in parallel by shapefile
  polys <- foreach(i = 1:length(shapefiles)) %dopar% {

    # pull shapefile
    shape <- shapefiles[i]
    message(paste0("Working on shapefile: ", shape))
    shp <- shapefile(paste0(shp_path, "/", shape, ".shp"))

    # subsetting will break if NAs in row index (GAUL_CODE)
    shp <- shp[!is.na(shp$GAUL_CODE), ]

    # get location codes as numeric, not factor
    loc_codes <- unique(shapes[shapes$shapefile == shape, ]$location_code) %>%
      as.character() %>%
      as.numeric()

    polys_subset <- list()

    for (j in 1:length(loc_codes)) {
      code <- loc_codes[j]

      if (code %in% shp$GAUL_CODE) {
        poly <- shp[shp$GAUL_CODE == code, ]
      } else {
        warning(sprintf("GAUL code: %s not found in shapefile: %s", code, shape))
        poly <- NULL
      }

      poly_name <- paste0(shape, "__", code)
      polys_subset[[poly_name]] <- poly
    }

    return(polys_subset)
  }

  polys <- unlist(polys)

  # find ones that didn't work
  bad_records <- which(sapply(polys, class) == "NULL")
  if (length(bad_records) != 0) {
    warning(sprintf("%i polygons could not be found:", length(bad_records)))
    print(names(bad_records))
    if (!ignore_warnings) {
      stop("Since ignore_warnings==FALSE, the function is now breaking. Please fix your bad records or set ignore_warnings==TRUE to drop them.\n")
    } else {
      warning("Since you have set ignore_warnings=TRUE, the function will drop all bad records and continue. Note: this is NOT recommended.\n")
      data <- data[!paste(data$shapefile, data$location_code, sep = "__") %in%
        names(bad_records), ]
    }
  }

  ######################################
  ##
  message("In parallel, generating integration points for each shapefile and expanding records to integrate. \n")

  data <- as.data.table(data)

  ## get unique sets of shapefiles and location codes (bins optional)
  if (is.null(raster_dict)) {
    d <- data[, c(unique_vars, "shapefile", "location_code")]
    pars <- unique(d)
    pars <- pars[!is.na(pars$shapefile), ]
    n_chunk <- nrow(pars)
  } else {
    d <- data[, c(unique_vars, "shapefile", "location_code", raster_col), with = FALSE]
    pars <- unique(d)
    pars <- pars[!is.na(pars$shapefile), ]
    n_chunk <- nrow(pars)
  }

  ## get row indices
  message("Getting indices and chunking out data by polygon in parallel.\n")
  dx <- trim(apply(d, 1, paste, collapse = "--"))
  px <- trim(apply(pars, 1, paste, collapse = "--"))

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
  suppressMessages(suppressWarnings(load_simple_polygon(
    gaul_list = (regions), buffer = 0.4, subset_only = TRUE,
    shapefile_version = shapefile_version
  ))) # appears to be hard coded for africa, changing to sri lanka for now
  raster_list <- suppressMessages(suppressWarnings(build_simple_raster_pop(subset_shape)))
  if (use_1k_popraster) {
    popraster <- disaggregate(raster_list[["pop_raster"]], 5) # needs to be 1km for density 0.001
    # popraster=brick('/share/geospatial/pop_density/africa_1km_pop.tif')
  } else {
    popraster <- raster_list[["pop_raster"]]
  }

  ## load other rasters if need be
  if (!is.null(raster_dict)) {
    message("Loading non-population rasters")
    ## raster_list <- NULL
    for (rr in 1:nrow(raster_dict)) {
      raster_list[[as.character(raster_dict[rr, 1])]] <- raster(as.character(raster_dict[rr, 2]))
    }
  }

  ## get points in parallel -----------------
  message("Running getPoints() on each polygon in parallel.\n")
  if (is.null(raster_dict)) {
    getPointsWrapper <- function(x) {
      poly_name <- paste(pars[x, c("shapefile", "location_code")], collapse = "__")
      poly <- polys[[poly_name]]

      ## NOTE: simply resampling on 2010 population data for now for convenience. Could redo by year, but wouldnt make much of difference anyway.
      if (is.null(poly)) {
        ## make a dummy, empty points dataframe
        points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
      } else {
        points <- try(
          getPoints(
            shape = poly,
            raster = popraster[[3]],
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
  } else {
    getPointsWrapper.w.dict <- function(x) {
      which.rast <- as.character(x[2])
      x <- as.numeric(x[1])
      poly_name <- paste(pars[x, c("shapefile", "location_code")], collapse = "__")
      poly <- polys[[poly_name]]

      ## NOTE: simply resampling on 2010 population data for now for convenience. Could redo by year, but wouldnt make much of difference anyway.
      if (is.null(poly)) {
        ## make a dummy, empty points dataframe
        points <- data.frame(longitude = NA, latitude = NA, weight = NA)[0, ]
      } else {
        rast <-
          points <- try(
            getPoints(
              shape = poly,
              raster = (if (length(raster_list[[which.rast]]) == 1) raster_list[[which.rast]][[1]] else raster_list[[which.rast]][[1]]),
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
    ## match raster_dict to n_chunk and make a matrix to apply over
    n.rast <- data.frame(n = 1:n_chunk, rast = pars[, raster_col, with = FALSE])
    chunk_points <- apply(n.rast, 1, getPointsWrapper.w.dict)
  }

  # duplicate records, and add their integration points
  message("Duplicating polygon records for each of their new integration points.\n")
  getNewChunksWrapper <- function(x) {
    # data for this chunk (shapefile/age bin combo)
    chunk <- chunks[[x]]
    points <- chunk_points[[x]]

    if (nrow(points) == 0) {
      new_chunk <- chunk[0, ]
      warning(paste("Chunk", x, "missing spatial info and will be dropped."))
    } else {
      dupRecords <- function(j) {
        record <- chunk[j, , drop = FALSE] # pull out the record
        # drop columns also in "points"
        record <- record[, !(colnames(record) %in% colnames(points)), with = FALSE]
        # faster than rbind / replicate
        record_dup <- cbind(record, points)

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
  # new_chunks_all <- do.call(rbind, new_chunks)
  # rbindlist() is the newer version that does the same thing as do.call(rbind,). Also, fill = TRUE to account for bad polygon producing an empty dataframe with a different number of columns than successful ones.
  # new_chunks_all <- rbindlist(new_chunks, fill = TRUE)
  # new_chunks_all$pseudocluster = TRUE
  # data$pseudocluster= FALSE
  # data <- rbind(data, new_chunks_all)
  new_chunks_all <- rbindlist(new_chunks, fill = TRUE)
  new_chunks_all$pseudocluster <- TRUE
  # if(length(data$start_year!=0)) {
  data$pseudocluster <- FALSE
  data <- rbind(data, new_chunks_all, fill = TRUE)
  # } else {
  #    data <- new_chunks_all
  #  }

  # count rows with no lat/longs and remove them
  no_ll <- which(is.na(data$latitude) | is.na(data$longitude))
  message(paste("Dropping", length(no_ll), "rows with missing spatial information."))
  if (length(no_ll) != 0) data <- data[-no_ll, ]

  # remove the shapefile and location_code columns
  # data <- data[, !(colnames(data) %in% c('shapefile', 'location_code'))]


  stopCluster(cl)

  data <- data.table(data)
  return(data)
}
