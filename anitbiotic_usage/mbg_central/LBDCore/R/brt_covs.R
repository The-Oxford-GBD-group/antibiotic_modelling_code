#' @title BRT Covariates
#' @description Takes Covariates and Data and returns Boosted Regression Trees Outputs
#'
#' @note depends on \code{gbm} and \code{dismo} packages
#'
#' @param df data.table with variables "latitude", "longitude", "N", and specified indicator
#' @param lcovs list of raster layers or bricks (for temporally varying) output of load_and_crop_covariates()
#' @param years analysis years. defaults to 2000, 2005, 2010, 2015
#' @param weight character of weight variable name in df, defaults to null
#' @param tc tree.complexity for BRT, defaults to 4
#' @param lr learning.rate for BRT, defaults to 0.005
#' @param bf bagging.fraction for BRT, defaults to 0.75
#' @param return_only_raster: if TRUE, only returns raster of results, otherwise returns a list of BRT model object and prediction raster
#'
#' @return A raster or a brick of rasters (see above)
#' @export
brt_covs <- function(df,
                     indicator_family = indicator_family,
                     lcovs = cov_layers,
                     years = c(2000, 2005, 2010, 2015),
                     weight = NULL,
                     tc = 4,
                     lr = 0.005,
                     bf = 0.75,
                     return_only_raster = TRUE) {

  # take lcovs, see which are bricks (time varying) and which arent
  ntv <- tv <- c()
  for (i in 1:length(lcovs))
    if (inherits(lcovs[[i]], "RasterLayer")) ntv <- c(ntv, i) else tv <- c(tv, i)


  # make sure the tv covariates have the same number of years as the years argument
  for (i in tv) {
    y <- dim(lcovs[[i]])[3]
    if (y == length(years)) {
      message(sprintf("The time-varying covariate `%s` has %i years of data. Matching to argument `years`: %s. With layer 1 as earliest year. If this is an issue, please rearrange your input RasterBrick for time-varying covariates. \n\n", names(lcovs)[i], y, paste(years, collapse = ", ")))
    } else {
      stop(sprintf("The time-varying covariate `%s` has %i years of data. Which does not match the number of years in argument `years`: %s.", names(lcovs)[i], y, paste(years, collapse = ", ")))
    }
  }

  # run BRT by years
  message(sprintf("Running BRT on %i years of data independently. Result will be a RasterBrick with %i layers.", length(years), length(years)))

  # registerDoMC(cores=length(years))
  # out <- foreach(i = 1:length(years),
  #        .packages=c('dismo', 'gbm', 'raster'),
  #        .export=c('df','years','indicator_family','weight','ntv','tv','lcovs')
  #        ) %dopar% {
  x <- list()
  for (i in 1:length(years)) {
    # subset only to years we need
    d <- df[df$year == years[i], ]
    d <- na.omit(d)

    # keep only the variables of interest
    coords <- d[, c("longitude", "latitude"), with = FALSE]
    coords$latitude <- as.numeric(coords$latitude)
    coords$longitude <- as.numeric(coords$longitude)

    # BRT function we use has no binomial, only pois with offset
    if (indicator_family %in% c("binomial", "poisson")) {
      indicator_family <- "poisson"
      offset <- log(d$N)
      message("WARNING: For Poisson to work, need to round decimals in the response")
      d[, eval(indicator) := round(d[, eval(indicator), with = FALSE], 0)]
    } else {
      offset <- NULL
    }

    d <- d[, c(indicator, weight), with = FALSE]

    # extract the values of the covariates
    c <- brick(lcovs[ntv])
    for (j in tv)
      c <- addLayer(c, lcovs[[j]][[i]])
    d <- cbind(d, extract(c, coords))


    # learning brt
    set.seed(123)
    # TODO: throw a try-catch so if some years work it at least will return that, if it doesnt it will try different things (like changing the learning rate. )
    mod <- try(
      dismo::gbm.step(
        data = d,
        gbm.y = 1,
        gbm.x = names(c),
        offset = offset,
        family = indicator_family,
        weights = weight,
        tree.complexity = tc,
        learning.rate = lr,
        bag.fraction = bf
      ),
      silent = TRUE
    )
    if (is.null(mod)) {
      message("First BRT attempt failed. Lowering Learning Rate by 1/10")
      mod <- try(
        dismo::gbm.step(
          data = d,
          gbm.y = 1,
          gbm.x = names(c),
          offset = offset,
          family = indicator_family,
          weights = weight,
          tree.complexity = tc,
          learning.rate = lr * .1,
          bag.fraction = bf
        )
      )
    }
    if (is.null(mod)) {
      message("Second BRT attempt failed. Lowering Original Learning rate by 1/1000 AGAIN")
      mod <- try(
        dismo::gbm.step(
          data = d,
          gbm.y = 1,
          gbm.x = names(c),
          offset = offset,
          family = indicator_family,
          weights = weight,
          tree.complexity = tc,
          learning.rate = lr * .001,
          bag.fraction = bf
        )
      )
    }
    if (is.null(mod)) {
      message("Third BRT attempt failed. Slow learn plus low tree complexity")
      mod <- try(
        dismo::gbm.step(
          data = d,
          gbm.y = 1,
          gbm.x = names(c),
          offset = offset,
          family = indicator_family,
          weights = weight,
          tree.complexity = 2,
          learning.rate = lr * .001,
          bag.fraction = bf
        )
      )
    }

    if (is.null(mod)) stop("ALL BRT ATTEMPTS FAILED")

    # save prediction rasters
    p <- predict(c, mod, n.trees = mod$gbm.call$best.trees, type = "response")

    # save the outputs
    x[[paste0("m_", i)]] <- mod
    x[[paste0("brt.", i)]] <- p
    # x
  } # closes years parallel loop


  # extract objects and prediction surfaces as two separate lists and save them
  for (i in 1:length(x))
    assign(names(x)[i], x[[i]])

  m <- (mget(paste0("m_", 1:length(years))))
  p <- (mget(paste0("brt.", 1:length(years))))



  if (return_only_raster) {
    return(brick(p))
  } else {
    return(list(m, brick(p)))
  }
}
