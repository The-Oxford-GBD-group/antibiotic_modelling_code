#' @title Covariate loader for standard Model-based Geostatistics.
#'
#' @description Loads covariate data in bulk and returns suitable raster/brick objects.
#'
#' @details
#' Covariates are stored in a 00_MBG_STANDARD directory. See
#' \code{MbgStandardCovariateLoader$public_fields$cov_dir} or an instances $cov_dir attribute
#' as .tif files and loaded as raster objects (either as a layer or a brick of layers).
#'
#' @param start_year A numeric indicating the earliest year of data to attempt to retrieve.
#' @param end_year A numeric indicating the latest year of data to attempt to retrieve.
#' @param interval A numeric number of months that the data is provided in. Annnual data
#'  would have an interval of 12
#' @param covariate_config A data.table with covariate loading information. This must contain
#'  columns "covariate", "measure", and "release". All three should be string values with
#'  the "release" being a timestamp in the form of YYYY_MM_DD e.g., "2019_06_13".
#'
#' @examples
#' \dontrun{
#' config.table <- data.table(
#'   covariate = c("access", "evi"),
#'   measure = c("mean", "median"),
#'   release = c("2019_06_10", "2019_06_10")
#' )
#'
#' start_year <- 1998
#' end_year <- 2017
#' interval_mo <- 60
#' template_raster <- suppressWarnings(empty_world_raster())
#'
#' loader <- MbgStandardCovariateLoader$new(
#'   start_year = start_year,
#'   end_year = end_year,
#'   interval = interval_mo,
#'   covariate_config = config.table
#' )
#' # all loaded covariates will be cropped and masked to the provided template raster.
#' lcc <- loader$get_covariates(template_raster)
#' }
#'
#' @rdname MbgStandardCovariateLoader
#' @export
MbgStandardCovariateLoader <- R6::R6Class("MBGStandardCovariateLoader",
  public = list(
    # init function
    initialize = function(start_year, end_year, interval, covariate_config, cov_dir = NULL) {
      private$start_year <- start_year
      private$end_year <- end_year
      private$interval <- interval
      private$covariate_config <- covariate_config
      private$path_helper <- CovariatePathHelper$new()
      if (!is.null(cov_dir)) {
        private$path_helper$cov_dir <- cov_dir
      }
      self$validate() # performs additional assignments
    },
    validate = function() {
      if (!dir.exists(private$path_helper$cov_dir)) {
        stop(sprintf("Covariate directory %s does not exist or is not accessible!", private$path_helper$cov_dir))
      }
      # validate interval
      if (!private$interval %in% private$valid_intervals) {
        stop(sprintf(
          "Only intervals %s supported. If you need monthly contact the LBD Core team",
          paste(private$valid_intervals, collapse = "/")
        ))
      }

      interval.years <- private$interval / 12
      private$all_periods <- seq(private$start_year, private$end_year, interval.years)

      private$duration <- paste0(interval.years, "y")

      measure.dirs <- private$path_helper$covariate_paths(
        covariates = private$covariate_config$covariate,
        measures = private$covariate_config$measure,
        releases = private$covariate_config$release
      )
      if (!all(dir.exists(measure.dirs))) {
        private$error_for_missing_covariates()
      }
      private$measure_dirs <- measure.dirs
      # TODO: can we check for duration/"synoptic" dirs and error faster IFF not present?
    },
    # public functions
    get_covariates = function(template_raster) {
      "returns a list of covariates, just like load_and_crop_covariates_annual"
      cov.list <- list()
      for (i in 1:nrow(private$covariate_config)) {
        covariate <- private$covariate_config[i, covariate]
        duration <- private$best_duration_dir(private$measure_dirs[i], private$duration)
        if (duration$is.synoptic) {
          # if duration is NOT "synoptic", get list of all missing periods
          # iff any periods are missing
          #   warn they are loading for years X and we have years Y
          #   warn that "we will map adjacent nearby years..."
        }
        if (duration$is.synoptic) {
          message(sprintf("Loading %s which is synoptic", covariate))
          rast <- private$load_synoptic_covariate(duration$dir, i)
        } else {
          message(sprintf("Loading %s which is not synoptic", covariate))
          rast <- private$load_covariate(duration$dir, i)
        }
        cropped <- raster::crop(rast, raster::extent(template_raster))
        cov.list[[covariate]] <- raster::mask(cropped, template_raster)
      }
      return(cov.list)
    }
  ), # end public
  # private functions
  private = list(
    path_helper = NULL,
    start_year = NULL,
    end_year = NULL,
    interval = NULL, # in months
    valid_intervals = c(12, 24, 60),
    duration = NULL, # e.g., '5y'/ '2y' / '1y'
    covariate_config = NULL, # data.table
    measure_dirs = NULL,
    all_periods = NULL,

    best_duration_dir = function(measure.dir, duration) {
      # ideal: data exists for requested covariate/measure/duration
      dir <- file.path(measure.dir, duration)
      if (dir.exists(dir)) {
        return(list(dir = dir, is.synoptic = FALSE))
      }

      meta <- private$path_helper$covariate_metadata_from_path(measure.dir)

      # acceptable: data exists for covariate/measure as synoptic data
      dir <- file.path(measure.dir, "synoptic")
      if (dir.exists(dir)) {
        message(sprintf("%s measure (%s / %s) is synoptic only", meta$measure, meta$covariate, meta$release))
        return(list(dir = dir, is.synoptic = TRUE))
      }

      # error: no data available
      measure <- basename(measure.dir)
      err.msg <- paste(
        duration, "duration for measure", measure, "for covariate", covariate,
        "does not exist and is not synoptic"
      )
      stop(err.msg)
    },
    load_covariate = function(dir, i) {
      rasters <- list()
      covariate <- private$covariate_config[i, covariate]
      measure <- private$covariate_config[i, measure]

      periods <- private$get_periods(dir, private$all_periods)
      n_periods <- length(private$all_periods)
      for (period.index in 1:n_periods) {
        period <- private$all_periods[period.index]
        if (period %in% periods$missing) {
          best.period <- private$get_closest_period(period, periods$present)
          msg <- sprintf(
            "WARNING! We are substituting in %s data from period: %i to use as if it were for period: %i",
            covariate, best.period, period
          )
          message(msg)
        } else {
          best.period <- period
        }
        best.file <- sprintf("%s_%s_%s_%i_00_00.tif", covariate, measure, private$duration, best.period)
        best.path <- file.path(dir, best.file)
        rasters[[period.index]] <- raster::raster(best.path) # BREAKING THINGS
      }
      result <- raster::stack(rasters[1:period.index])
      names(result) <- rep(paste0(covariate, ".", 1:n_periods))
      return(result)
    },
    load_synoptic_covariate = function(dir, i) {
      covariate <- private$covariate_config[i, covariate]
      measure <- private$covariate_config[i, measure]
      path <- file.path(
        dir,
        paste(covariate, measure, "synoptic.tif", sep = "_")
      )

      if (!file.exists(path)) {
        err.msg <- paste("Searched for the following file and it does not exist:", path)
        stop(err.msg)
      }
      result <- raster::raster(path)
      names(result) <- covariate
      return(result)
    },
    get_periods = function(dir, expected_periods) {
      # background: files have very explicit filenames e.g., "cruststmn_median_5y_2000_00_00"
      # this is COVARIATE_MEASURE_DURATION_YEAR_MONTH_DAY

      # get files with duration (e.g., "5y") in name. Others should be ignored
      files <- list.files(dir)
      files <- files[grep(private$duration, files)]
      # strip extension, get unique values
      base_names <- unique(unlist(lapply(files, private$filename_without_extension)))
      # extract YEAR (third to last value) and convert to numeric
      periods <- as.numeric(sort(unlist(lapply(
        strsplit(base_names, split = "_", fixed = TRUE),
        function(pieces) {
          pieces[length(pieces) - 2]
        }
      ))))

      missing.periods <- setdiff(expected_periods, periods)
      if (length(missing.periods) > 0) {
        message("WARNING! You are trying to load a raster covariate but the following years are missing:")
        message(paste(missing.periods, collapse = " ", sep = ""))
        message("WARNING! We will map adjacent nearby years to these missing periods to fill in your dataset")
      }
      return(list(present = periods, missing = missing.periods))
    },
    get_closest_period = function(desired, available) {
      distance <- abs(desired - available)
      available[which.min(distance)]
    },
    filename_without_extension = function(f) {
      # returns filename without extension
      # unlike tools::file_path_sans_exit this will remove ALL extensions, not the first
      # e.g., foo.bar.baz becomes foo, not foo.bar
      strsplit(f, ".", fixed = TRUE)[[1]][1]
    },
    error_for_missing_covariates = function() {
      # TODO: should we build one big error message instead of a staged one?
      #       right now a user might have to run this 3 times to find all their errors:
      #       missing covariates, missing releases, missing measures

      # test for requested COVARIATES which do not exist
      cov.dirs <- private$path_helper$covariate_paths(covariates = private$covariate_config$covariate)
      if (!all(dir.exists(cov.dirs))) {
        missing.index <- which(!dir.exists(cov.dirs))
        covariates <- private$covariate_config[missing.index, covariate]
        msg <- paste(
          "You have selected some covariates in fixed_effects which do not exist:",
          paste0(covariates, collapse = ", ")
        )
        stop(msg)
      }

      # test for requested MEASURES which do not exist
      measure.dirs <- private$path_helper$covariate_paths(
        covariates = private$covariate_config$covariate,
        measures = private$covariate_config$measure
      )
      if (!all(dir.exists(measure.dirs))) {
        missing.index <- which(!dir.exists(measure.dirs))
        covariates <- private$covariate_config[missing.index, covariate]
        measures <- private$covariate_config[missing.index, measure]
        msg <- paste(
          "The following measures for covariates do not exist:",
          paste(measures, " (", covariates, ")", sep = "", collapse = "; ")
        )
        stop(msg)
      }

      # test for requested RELEASES which do not exist
      release.dirs <- private$path_helper$covariate_paths(
        covariates = private$covariate_config$covariate,
        measures = private$covariate_config$measure,
        releases = private$covariate_config$release
      )
      if (!all(dir.exists(release.dirs))) {
        missing.index <- which(!dir.exists(release.dirs))
        covariates <- private$covariate_config[missing.index, covariate]
        measures <- private$covariate_config[missing.index, measure]
        releases <- private$covariate_config[missing.index, release]
        msg <- paste(
          "The following releases for covariate / measure do not exist:",
          paste(releases, " (", covariates, " / ", measures, ")", sep = "", collapse = "; ")
        )
        stop(msg)
      }
    }
  ) # end private
)
