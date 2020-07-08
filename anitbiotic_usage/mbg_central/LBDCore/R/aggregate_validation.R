#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param holdoutlist PARAM_DESCRIPTION, Default: stratum_qt
#' @param cell_draws_filename PARAM_DESCRIPTION, Default: '\%s_cell_draws_eb_bin\%i_\%s_\%i\%s.RData'
#' @param years PARAM_DESCRIPTION, Default: \code{c(2000, 2005, 2010, 2015)}
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param holdout PARAM_DESCRIPTION
#' @param vallevel PARAM_DESCRIPTION, Default: ''
#' @param addl_strat PARAM_DESCRIPTION, Default: \code{c(age = 0)}
#' @param sbh_wgt PARAM_DESCRIPTION, Default: NULL
#' @param returnaggonly PARAM_DESCRIPTION, Default: TRUE
#' @param iter PARAM_DESCRIPTION, Default: i
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname aggregate_validation
#' @export
#' @importFrom raster extract
aggregate_validation <- function(holdoutlist = stratum_qt,
                                 cell_draws_filename = "%s_cell_draws_eb_bin%i_%s_%i%s.RData",
                                 years = c(2000, 2005, 2010, 2015),
                                 indicator_group,
                                 indicator,
                                 run_date,
                                 reg,
                                 holdout,
                                 vallevel = "",
                                 addl_strat = c("age" = 0),
                                 sbh_wgt = NULL,
                                 returnaggonly = TRUE,
                                 iter = i) {

  ##################################################################################################
  ##################################################################################################
  ## Overview:  Will search saved results and model objects to produce m draws of estimates
  #             for each holdout area. Note this is written as a unit (ie for one reg, holdout),
  #             an will likely be run in a loop or apply function
  #
  ## Inputs:
  #          indicator_group: you know
  #          indicator: oh you know
  #          run_date: you definitely know
  #          reg: string, region abbreviation, or just 'africa'
  #          addl_strat: ie c('age'=1), make age=0 for others
  #          holdout: holdout number
  #
  ## Outputs: A table with one row per area-holdout, with p as estimated from data
  #           and m draws of p_hat for each area

  ## Load data
  datdir <- sprintf("/share/geospatial/mbg/%s/%s/output/%s/", indicator_group, indicator, run_date)

  # cell draws
  cdfile <- sprintf(paste0(datdir, cell_draws_filename), indicator, addl_strat, reg, holdout, vallevel)
  # message(paste0('Loading cell draws from ',cdfile))
  load(cdfile) # loads cell pred

  # holdout data
  d <- data.frame(holdoutlist[[sprintf("region__%s___%s__%i", reg, names(addl_strat), addl_strat)]])
  if (holdout != 0) {
    d_oos <- d[d$fold == holdout, ]
  } else {
    d_oos <- d
    d_oos$fold <- 0
  }
  # Acct for weights
  d_oos$indic_orig <- d_oos[, indicator]
  if (is.null(sbh_wgt)) {
    d_oos$exposure <- d_oos$N * d_oos$weight
    d_oos[, indicator] <- d_oos[, indicator] * d_oos$weight
  } else { # either a variable set (char) or numeric auto down weight
    if (class(sbh_wgt) == "character") {
      d_oos$exposure <- d_oos$N * d_oos$weight * d_oos[, sbh_wgt]
      d_oos[, indicator] <- d_oos$died * d_oos$weight * d_oos[, sbh_wgt]
    }
    if (class(sbh_wgt) == "numeric") {
      d_oos$exposure <- d_oos$N * d_oos$weight * sbh_wgt
      d_oos[, indicator] <- d_oos[, indicator] * d_oos$weight * sbh_wgt
    }
  }


  draws <- dim(cell_pred)[2]

  # load simple raster
  load(paste0("/share/geospatial/shapefiles/simple_raster", reg, ".RData"))

  # for each draw in cell_draws, pull the p
  # message(paste('Pulling estimated rates for',dim(cell_pred)[2],'draws into rasters.'))
  r_list <- list()
  #  pb <- txtProgressBar(style = 3)
  for (i in 1:draws) {
    #    setTxtProgressBar(pb, i / draws)
    r_list[[length(r_list) + 1]] <- insertRaster(simple_raster, matrix(cell_pred[, i], ncol = length(years)))
  }
  #  close(pb)

  # extract probabilities from draw rasters
  # message(paste('Extracting estimated probabilities at data locations for',draws,'draws rasters.'))
  ycol <- match(d_oos$year, years)
  # pb <- txtProgressBar(style = 3)
  for (i in 1:draws) {
    # extract at locations
    t <- raster::extract(r_list[[i]], cbind(d_oos$longitude, d_oos$latitude))
    # match year and put into df
    d_oos[, paste0(indicator, "hat_", i)] <- sapply(seq_along(ycol), function(x) {
      t[x, ycol[x]]
    }) * d_oos$exposure
    d_oos[, paste0(indicator, "hat_full_", i)] <- sapply(seq_along(ycol), function(x) {
      t[x, ycol[x]]
    }) * d_oos$N
  }
  # close(pb)

  # get binomial draws for each point-draw, and estimate the coverage based on X%ci
  # message('Doing coverage using binomial draws. ') # NAs happen if points were in water
  x <- matrix(NA, ncol = draws, nrow = nrow(d_oos))
  for (i in 1:draws) {
    x[, i] <- rbinom(nrow(d_oos),
      size = round(d_oos$N, 0),
      prob = d_oos[, paste0(indicator, "hat_full_", i)] / d_oos$N
    )
  }
  for (c in c(50, 80, 95)) {
    coverage <- c / 100
    li <- apply(x, 1, quantile, p = (1 - coverage) / 2, na.rm = T)
    ui <- apply(x, 1, quantile, p = coverage + (1 - coverage) / 2, na.rm = T)
    d_oos[, paste0("clusters_covered_", c)] <- d_oos[, "indic_orig"] >= li & d_oos[, "indic_orig"] <= ui
  }

  # collapse into areas
  d_oos[, names(addl_strat)] <- addl_strat
  d_oos$total_clusters <- 1
  # message(names(d_oos))
  res <- d_oos[c(names(addl_strat), "fold", "ho_id", "year", indicator, "exposure", paste0("clusters_covered_", c(50, 80, 95)), "total_clusters", paste0(indicator, "hat_", 1:draws))]
  f <- as.formula(paste(".~ho_id+year+fold", names(addl_strat), sep = "+"))
  resfull <- copy(res)
  res <- aggregate(f, data = res, FUN = sum)

  # transform back to probability
  res$p <- res[, indicator] / res$exposure
  res$region <- reg
  res$oos <- res$fold != 0
  for (i in 1:draws) {
    res[, paste0("phat_", i)] <- res[, paste0(indicator, "hat_", i)] / res$exposure
    res[, paste0(indicator, "hat_", i)] <- NULL
  }
  res[, indicator] <- NULL

  message(sprintf("%i is finished in function as %s", iter, paste0(class(res), collapse = " ")))


  ## return results table
  if (returnaggonly == TRUE) {
    return(data.table(res))
  } else {
    # transform back to probability
    resfull$p <- resfull[, indicator] / resfull$exposure
    resfull$region <- reg
    resfull$oos <- resfull$fold != 0
    for (i in 1:draws) {
      resfull[, paste0("phat_", i)] <- resfull[, paste0(indicator, "hat_", i)] / resfull$exposure
      resfull[, paste0(indicator, "hat_", i)] <- NULL
    }
    resfull[, indicator] <- NULL
    return(list(agg = data.table(res), cluster = data.table(resfull)))
  }
}
