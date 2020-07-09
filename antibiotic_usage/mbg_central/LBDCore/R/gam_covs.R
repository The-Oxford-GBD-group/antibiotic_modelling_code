#' @title GAM Covariates
#' @description Take input data and covariate layers and fit GAM models
#' @param df  data.table with variables "latitude", "longitude", "N", and specified indicator
#' @param indicator_family the type of likelihood function, either \code{"binomial"} or \code{"gaussian"}
#' @param lcovs  list of raster layers or bricks (for temporally varying). Currently assumes
#'             four layers named like U5M periods.
#' @return Two-item list (bricks of time-varying and non-time-varying covariates)
#' @note This function currently won't work if you have anything other than *four* distinct periods in your data and covariates.
#'           This will be made more flexible.
#' @export
gam_covs <- function(df, indicator_family, lcovs) {
  coords <- df[, c("longitude", "latitude"), with = FALSE]
  coords$lat <- as.numeric(coords$latitude)
  coords$long <- as.numeric(coords$longitude)
  coords <- coords[, c("long", "lat"), with = FALSE]

  if (indicator_family == "binomial") response <- cbind(died = df[, get(indicator)], lived = df[, N] - df[, get(indicator)])
  if (indicator_family == "gaussian") response <- cbind(outcome = df[, get(indicator)])

  extra_data <- data.frame(year = df$year)

  # fit gam
  # This should take a few minutes
  system.time(trans <- gamTrans(
    coords = coords,
    response = response,
    covs = lcovs,
    family = indicator_family,
    extra_terms = ~year,
    extra_data = extra_data,
    bam = TRUE,
    predict = TRUE,
    condition = NULL,
    condition_covs = NULL,
    s_args = list(bs = "ts", k = 3),
    samfrac = 0.1,
    use.chol = TRUE
  ))

  ### IF trans$trans IS RETURNED AS A LIST, IT IS SPLIT INTO TEMPORAL AND NON-TEMPORAL COVARIATES
  if (class(trans$trans) == "list") {
    temporal <- TRUE
  } else {
    temporal <- FALSE
  }

  message("CLAMP AND SAVE")
  if (!temporal) {
    # THEY ALL PASS
    # use chi-squared stats to determine covariate usefulness
    keep <- which(summary(trans$model)$chi.sq > 0.1)
    trans_ras <- trans$trans[[keep]]

    # clamp covariates
    # find most extreme vaaues of transofmred covariates that were observed
    vals <- extract(trans_ras, coords[idx_fit, ])
    sry <- apply(vals, 2, range, na.rm = TRUE)

    # clamp the covariates to these values
    for (i in 1:nlayers(trans_ras)) {
      range <- sry[, colnames(sry) == names(trans_ras)[i]]
      trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
      trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
    }

    return(trans_ras)
  }

  # temporally varying covariates are present, save them all separately
  # non varying ones will be save in the same covs_transformed location as before
  if (temporal) {

    # first clamp and save non temporally varying
    message("time invariant covariates")
    trans_ras <- trans$trans$nT_vals_trans
    # clamp covariates
    # find most extreme vaaues of transofmred covariates that were observed
    vals <- extract(trans_ras, coords)
    sry <- apply(vals, 2, range, na.rm = TRUE)


    # clamp the covariates to these values
    for (i in 1:nlayers(trans_ras)) {
      range <- sry[, colnames(sry) == names(trans_ras)[i]]
      trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
      trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
    }

    # If you only specify one non-varying term, it simply gets name "layer" in the GAM function. Rather than fixing it in there
    # I'm just going to check if that's the case and rename it here.
    all_rasters <- list()
    if (length(names(trans_ras)) == 1) {
      for (cov in c("access", "irrigation", "elevation", "africa_lf")) {
        if (cov %in% names(lcovs)) {
          names(trans_ras) <- cov
          all_rasters[[cov]] <- trans_ras
        }
      }
    }

    # Now, this same process for the individual temporally varying covariates
    count <- length(all_rasters) + 1
    for (n in names(trans$trans$T_vals_trans)) {
      message(n)
      message(count)

      trans_ras <- trans$trans$T_vals_trans[[n]]

      # clamp covariates
      # find most extreme vaaues of transformed covariates that were observed
      vals <- extract(trans_ras, coords)
      sry <- apply(vals, 2, range, na.rm = TRUE)


      # clamp the covariates to these values
      for (i in 1:nlayers(trans_ras)) {
        range <- sry[, colnames(sry) == names(trans_ras)[i]]
        trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
        trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
      }

      all_rasters[[n]] <- trans_ras
      count <- count + 1
    }

    return(all_rasters)
  }
}
