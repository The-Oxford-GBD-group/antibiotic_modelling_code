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
#' @rdname aggregate_validation_dev
#' @export
#' @importFrom raster extract
#' @importFrom seegMBG insertRaster
aggregate_validation_dev <- function(holdoutlist = stratum_qt,
                                     cell_draws_filename = "%s_cell_draws_eb_bin%i_%s_%i%s.RData",
                                     years = c(2000, 2005, 2010, 2015),
                                     indicator_group,
                                     indicator,
                                     run_date,
                                     reg,
                                     holdout,
                                     vallevel = "",
                                     addl_strat = c("age" = 0),
                                     iter = i) {

  ## Load data
  datdir <- sprintf("/share/geospatial/mbg/%s/%s/output/%s/", indicator_group, indicator, run_date)

  # cell draws
  cdfile <- sprintf(paste0(datdir, cell_draws_filename), indicator, addl_strat, reg, holdout, vallevel)
  # message(paste0('Loading cell draws from ',cdfile))
  load(cdfile)

  # holdout data
  d <- as.data.table(holdoutlist[[sprintf("region__%s", reg)]])
  if (holdout != 0) {
    d_oos <- d[d$fold == holdout, ]
  } else {
    d_oos <- d
    d_oos$fold <- 0
  }
  d_oos <- d_oos[, exposure := N * weight]
  d_oos <- d_oos[, (indicator) := get(indicator) * weight]
  draws <- dim(cell_pred)[2]
  # load simple raster
  load(paste0("/share/geospatial/shapefiles/simple_raster", reg, ".RData"))

  # for each draw in cell_draws, pull the p
  # message(paste('Pulling estimated rates for',dim(cell_pred)[2],'draws into rasters.'))
  r_list <- list()
  #  pb <- txtProgressBar(style = 3)
  for (i in 1:draws) {
    #    setTxtProgressBar(pb, i / draws)
    r_list[[length(r_list) + 1]] <- seegMBG::insertRaster(simple_raster, matrix(cell_pred[, i], ncol = length(years)))
  }
  #  close(pb)

  # extract probabilities from draw rasters
  # message(paste('Extracting estimated probabilities at data locations for',draws,'draws rasters.'))
  ycol <- match(d_oos$year, years)
  # pb <- txtProgressBar(style = 3)
  for (i in 1:draws) {
    #  setTxtProgressBar(pb, i / draws)require(package, lib.loc = package_lib, character.only=TRUE)
    # extract at locations
    t <- raster::extract(r_list[[i]], cbind(d_oos$longitude, d_oos$latitude))
    # match year and put into df
    d_oos[, paste0(indicator, "hat_", i)] <- sapply(seq_along(ycol), function(x) {
      t[x, ycol[x]]
    }) * d_oos$exposure
  }
  # close(pb)

  # get binomial draws for each point-draw, and estimate the coverage based on X%ci (95% for now)
  # message('Doing coverage using binomial draws. ')
  # get binomial draws for each point-draw, and estimate the coverage based on X%ci (95% for now)
  # message('Doing coverage using binomial draws. ')
  x <- matrix(rbinom(nrow(d_oos) * 100, size = round(d_oos$exposure, 0), prob = d_oos[, get(paste0(indicator, "hat_", i))] / d_oos$exposure), ncol = 100)
  for (c in c(50, 80, 95)) {
    coverage <- c / 100
    li <- apply(x, 1, quantile, p = (1 - coverage) / 2, na.rm = T)
    ui <- apply(x, 1, quantile, p = coverage + (1 - coverage) / 2, na.rm = T)
    d_oos <- d_oos[, li := li]
    d_oos <- d_oos[, ui := ui]
    d_oos[get(indicator) >= li & get(indicator) <= ui, covered := 1]
    d_oos[is.na(covered), covered := 0]
    setnames(d_oos, "covered", paste0("clusters_covered_", c))
  }


  # collapse into areas
  d_oos[, names(addl_strat)] <- addl_strat
  d_oos$total_clusters <- 1
  # message(names(d_oos))
  res <- d_oos[, c(names(addl_strat), "fold", "ho_id", "year", indicator, "exposure", paste0("clusters_covered_", c(50, 80, 95)), "total_clusters", paste0(indicator, "hat_", 1:draws)), with = F]
  f <- as.formula(paste(".~ho_id+year+fold", names(addl_strat), sep = "+"))
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
  return(data.table(res))
}
