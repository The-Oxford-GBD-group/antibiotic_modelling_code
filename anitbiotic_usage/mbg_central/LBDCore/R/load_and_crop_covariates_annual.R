#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param covs PARAM_DESCRIPTION
#' @param measures PARAM_DESCRIPTION
#' @param simple_polygon PARAM_DESCRIPTION
#' @param start_year PARAM_DESCRIPTION, Default: 1998
#' @param end_year PARAM_DESCRIPTION, Default: 2017
#' @param interval_mo PARAM_DESCRIPTION, Default: 60
#' @param agebin PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{mask}}
#' @rdname load_and_crop_covariates_annual
#'
#' @export
#'
#' @importFrom raster mask stack crop
load_and_crop_covariates_annual <- function(covs, # covs     = c('evi','lstday','irrigation')
                                            measures, # measures = c('median','median','mean')
                                            simple_polygon,
                                            start_year = 1998,
                                            end_year = 2017,
                                            interval_mo = 60,
                                            agebin = 1) {

  # covariate directory
  covdir <- "/home/j/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/"

  # pull a vector of all available covariates
  all_covs <- list.dirs(path = covdir, full.names = TRUE, recursive = FALSE)
  all_covs <- gsub(paste0(covdir, "/"), "", all_covs)

  # duration
  if (interval_mo %in% c(12, 24, 60)) {
    inyrs <- TRUE
    dur <- paste0(interval_mo / 12, "y")
    pers <- length(seq(start_year, end_year, interval_mo / 12))
  } else {
    dur <- paste0(interval_mo, "m")
    stop("THIS FUNCTION IS NOT CURRENTLY SET UP TO DO MONTHLY, CONTACT LBD CENTRAL CODE.")
  }
  message(paste0("Duration set to ", dur, "."))
  message(paste0("Will search for covariates starting in ", start_year, " and ending in ", end_year, "."))
  message(paste0("Interval years are:", paste(seq(start_year, end_year, interval_mo / 12), collapse = ", "), ".\n"))

  ## make vector of all periods
  all.pers <- seq(start_year, end_year, interval_mo / 12)

  ## CHECK: Covs and measures are of the same length
  if (class(covs) != "character") stop("covs argument must be a character vector")
  if (class(measures) != "character") stop("measuress argument must be a character vector")
  if (length(covs) != length(measures)) stop("covs and measures vectors must be of same length")

  ## CHECK: look through fixed effects and make sure we have covariates avaiilable
  mismatch <- covs[!covs %in% all_covs]
  if (length(mismatch) > 0) {
    stop(paste("You have selected some covariates in fixed_effects which do not exist:\n", paste0(mismatch, collapse = ", ")))
  }

  ## Pull covariates
  i <- 1
  covlist <- list()
  message("Finding your covariates and  searching for preferred measures and durations...\n")
  for (c in covs) {
    message(paste(c, "\n"))
    durtmp <- dur
    perstmp <- pers

    ## check for measure preference
    if (!dir.exists(paste0(covdir, c, "/", measures[i], "/"))) {
      stop(paste("measure", measures[i], "for", c, "does not exist."))
    }

    ## check for duration preference
    if (!dir.exists(paste0(covdir, c, "/", measures[i], "/", dur, "/"))) {
      if (dir.exists(paste0(covdir, c, "/", measures[i], "/", "synoptic"))) {
        message(paste(c, "is synoptic only."))
        durtmp <- "synoptic"
        perstmp <- 1
      } else {
        stop(paste(dur, "duration for measure", measures[i], "for", c, "does not exist."))
      }
    }

    ## check if all years are available for the given
    ## cov-measure-duration combo.
    ## if some times are missing, copy over from the closest
    ## available time giving preference to earlier over later years
    ## when presented with ties
    if (durtmp != "synoptic") { ## if we have different rasters for different times
      ## get all available periods by checking available files in the raster library
      all.file.pers <- list.files(paste0(covdir, c, "/", measures[i], "/", durtmp, "/"))
      all.file.pers <- all.file.pers[grep(durtmp, all.file.pers)]
      ## these filenames are of the form:
      ## <covariate>_<measure>_<duration>_<period>.tif. We want to
      ## grab period by splitting off the file ending, and then
      ## splitting on underscores
      all.file.pers <- unique(unlist(lapply(strsplit(all.file.pers, split = ".", fixed = T), function(x) {
        x[1]
      })))
      all.file.pers <- as.numeric(sort(unlist(lapply(strsplit(all.file.pers, split = "_", fixed = T), function(x) {
        x[length(x) - 2]
      }))))

      ## check to see if we have all the ones we need/want
      if (length(setdiff(all.pers, all.file.pers)) > 0) { ## i.e. we're missing periods
        missing.pers <- setdiff(all.pers, all.file.pers)
        message("WARNING! You are trying to load a raster covariate but the following years are missing:")
        message(paste(missing.pers, collapse = " ", sep = ""))
        message("WARNING! We will map adjacent nearby years to these missing periods to fill in your dataset")
      } else {
        missing.pers <- NULL
      }
    }

    ## load data
    message("Loading in data...\n")
    covlist[[c]] <- list()
    for (p in 1:perstmp) {
      # deal with tv an ntv differently in naming
      if (durtmp != "synoptic") {
        this.yr <- all.pers[p]

        ## setup filepath to correct year, unless it's mising, then we overwrite with a neighbor period
        yrtmp <- paste0(durtmp, "_", start_year + (interval_mo / 12) * (p - 1), "_00_00")
        ## grab a non-missing period if the period is missing
        ## grab nearest existing neighbor - preference to older periods in case of tie
        if (!is.null(missing.pers)) {
          if (this.yr %in% missing.pers) {
            ## get distance to existing years
            per.dist <- abs(this.yr - all.file.pers)
            yrtmp <- paste0(durtmp, "_", all.file.pers[which.min(per.dist)], "_00_00")
            message(sprintf(
              "WARNING! We are substituting in %s data from period: %i to use as if it were for period: %i",
              c, all.file.pers[which.min(per.dist)], this.yr
            ))
          }
        }
      } else {
        yrtmp <- "synoptic"
      }
      f <- paste0(covdir, c, "/", measures[i], "/", durtmp, "/", c, "_", measures[i], "_", yrtmp, ".tif")
      if (!file.exists(f)) stop(paste("Searched for the following file and it does not exist:", f))
      covlist[[c]][[p]] <- raster(f)
    }

    ## collapse list to a rasterBrick
    covlist[[c]] <- raster::stack(covlist[[c]][1:perstmp])
    names(covlist[[c]]) <- rep(paste0(c, ".", 1:perstmp))

    ## convert to regular raster if synoptic
    if (durtmp == "synoptic") {
      # covlist[[c]]  <- raster(  covlist[[c]]  )
      covlist[[c]] <- covlist[[c]][[1]]
      names(covlist[[c]]) <- c
    }

    i <- i + 1 ## update measure index for next cov
  }

  # Make sure covariate layers line up with raster we are modeling over
  message("Cropping to simple_polygon")
  for (l in 1:length(covlist)) {
    message(names(covlist)[l])
    covlist[[l]] <- raster::crop(covlist[[l]], extent(simple_polygon))
    ## setting the extent to simple_polygon here can cause issues when trying to align the covs to simple_raster later...
    ## I'm 99.99% sure this is safe to remove - but it's been in our code for ages. I left it here in case we were wrong and it's actually needed - azimmer
    ## covlist[[l]]  <- setExtent(covlist[[l]], simple_polygon) #, keepres=TRUE, snap=TRUE)
    covlist[[l]] <- raster::mask(covlist[[l]], simple_polygon)
  }

  return(covlist)
}
