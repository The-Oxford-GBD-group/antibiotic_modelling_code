#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ind_gp PARAM_DESCRIPTION
#' @param ind PARAM_DESCRIPTION
#' @param rd PARAM_DESCRIPTION
#' @param ind_fm PARAM_DESCRIPTION, Default: 'binomial'
#' @param age PARAM_DESCRIPTION, Default: 0
#' @param nperiod PARAM_DESCRIPTION, Default: 16
#' @param yrs PARAM_DESCRIPTION, Default: 2000:2015
#' @param write.to.file PARAM_DESCRIPTION, Default: FALSE
#' @param get.oos PARAM_DESCRIPTION, Default: FALSE
#' @param year_col PARAM_DESCRIPTION, Default: 'original_year'
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[rgdal]{readOGR}}
#'  \code{\link[sp]{over-methods}},\code{\link[sp]{over}}
#'  \code{\link[raster]{extract}}
#' @rdname get_is_oos_draws
#' @export
get_is_oos_draws <- function(ind_gp,
                             ind,
                             rd,
                             ind_fm = "binomial",
                             age = 0,
                             nperiod = 16, # FIXME: superfluous, can just use yrs
                             yrs = 2000:2015,
                             write.to.file = FALSE,
                             get.oos = FALSE,
                             year_col = "original_year",
                             shapefile_version = "current",
                             ...) {


  ## ###################################################################
  ## get_is_oos_draws()
  ##
  ## this function makes a dataframe of all your data that went into the
  ## model and appends columns of draw values from cell_preds
  ##
  ## INPUT
  ##
  ##   ind_gp: indicator_group
  ##   ind:    indicator
  ##   rd:     run_date
  ##   ind_fm: indicator_family (binomial, gaussian)
  ##   nperiod: number of periods/years in model
  ##   years: vector of years. should be of length nperiod
  ##   get.oos: should we also get OOS extracted values?
  ##   write.to.file: if true writes final df to standard output dir
  ##   shapefile_version: String specifying shapefile version to pull
  ##
  ## OUTPUT
  ##
  ##   a data frame with:
  ##     nrow = number data observations
  ##     columns for each draw and some identifying columns:
  ##       holdout_id, value, sample size, region
  ##
  ## #####################################################################

  ###############
  ## Load data ##
  ###############

  message("Load input data used in model")

  # load regions that were used in modeling
  mod.dir <- sprintf("/share/geospatial/mbg/%s/%s/output/%s/", ind_gp, ind, rd)
  all.regions <- get_output_regions(mod.dir)

  # load raw data
  if (!get.oos) {
    df <- fread(paste0(mod.dir, "input_data.csv")) ## raw input data
    df <- merge_with_ihme_loc(df, shapefile_version = shapefile_version)
  } else {
    df <- rbindlist(lapply(readRDS(paste0(mod.dir, "stratum.rds")), data.table))
  }

  # rename year column for convenience
  setnames(df, year_col, "the_year_col")

  ###################
  ## Assign admins ##
  ###################

  message("Identify ad1 and ad2 membership")

  # load admin2 shapefile (also contains admin1)
  admin2_shapefile <- rgdal::readOGR(get_admin_shapefile(admin_level = 2, version = shapefile_version))
  for (v in grep("CODE", names(admin2_shapefile@data))) admin2_shapefile@data[[v]] <- as.numeric(as.character(admin2_shapefile@data[[v]]))

  # identify the admin2 (and by extension, the admin1) each point belongs to
  locs <- sp::SpatialPoints(cbind(df$longitude, df$latitude), proj4string = CRS(proj4string(admin2_shapefile)))
  adm.df <- sp::over(locs, admin2_shapefile)

  # for those that don't fall inside a polygon, assign them to the nearest polygon (this tends to happen on coastlines)
  # do this by country to speed things up and to make sure the point ends up at least in the correct admin0
  for (ctry in unique(df[is.na(adm.df[, 1]), GAUL_CODE])) {
    ii <- which(is.na(adm.df[, 1]) & df$GAUL_CODE == ctry)
    temp_shape <- admin2_shapefile[admin2_shapefile@data$ADM0_CODE == ctry, ]
    distmat <- gDistance(locs[ii], temp_shape, byid = T)
    jj <- apply(distmat, 2, which.min)
    adm.df[ii, ] <- temp_shape@data[jj, ]
    rm(ii, jj, temp_shape, distmat)
  }

  # copy admin information into df
  df$ad1 <- adm.df$ADM1_CODE
  df$ad2 <- adm.df$ADM2_CODE
  df$ad0 <- df$GAUL_CODE # for consistency...

  ###############
  ## Get draws ##
  ###############

  message("Get draws")

  # loop over regions
  df_all <- rbindlist(lapply(all.regions, function(rr) {
    message(paste("...Region:", rr))

    # load the simple raster template
    message("......load simple raster template")
    gaul_list <- get_adm0_codes(rr, shapefile_version = shapefile_version)
    simple_polygon_list <- load_simple_polygon(
      gaul_list = gaul_list, buffer = 0.4, subset_only = TRUE,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    raster_list <- build_simple_raster_pop(subset_shape)
    template <- raster_list[["simple_raster"]]

    # subset data to this region
    df.r <- df[region == rr, ]
    loc.r <- as.matrix(df.r[, list(longitude, latitude)])

    # 'nudge' coordinates to make them fall inside the nearest cell non-NA cell in template if they
    # are in an NA cell (this can happen near coastlines)
    check <- raster::extract(template, loc.r)
    miss <- which(is.na(check))
    for (ii in miss) {
      dist <- replace(raster::distanceFromPoints(template, loc.r[ii, ]), is.na(template), NA)
      loc.r[ii, ] <- raster::xyFromCell(template, raster::which.min(dist)[1])
    }

    # make a raster of cell_pred row ids
    id_raster <- insertRaster(template, matrix(1:(length(cellIdx(template)) * nperiod), ncol = nperiod))

    # get cell_pred row ids for each data point
    for (yy in 1:nperiod) {
      this_year <- which(df.r$the_year_col == yrs[yy])
      if (length(this_year) == 0) next
      df.r[this_year, cell_pred_id := raster::extract(id_raster[[yy]], loc.r[this_year, ])]
    }

    # if out of sample metrics are requested, duplicate the data to create separate rows for in and out of sample
    if (get.oos) {
      df.r <- rbind(df.r, cbind(df.r[, -"fold", with = F], fold = 0), use.names = T)
    } else {
      df.r[, fold := 0]
    }

    # loop over holdouts
    for (this_fold in sort(unique(df.r$fold))) {

      # load cell pred objects
      message(paste("......load cell preds for holdout", this_fold))
      load(paste0(mod.dir, sprintf("%s_cell_draws_eb_bin%i_%s_%i.RData", ind, age, rr, this_fold)))

      # extract draws
      df.r[fold == this_fold, paste0("draw", 1:ncol(cell_pred)) := as.data.table(cell_pred[cell_pred_id, ])]
    }

    # return combined draws and draws
    return(df.r)
  }))

  # rename year column back to original
  setnames(df_all, "the_year_col", year_col)

  # save combined data and draws to file and return
  if (write.to.file) write.csv(df_all, paste0(mod.dir, "output_draws_data.csv"), row.names = F)
  return(df_all)
}
