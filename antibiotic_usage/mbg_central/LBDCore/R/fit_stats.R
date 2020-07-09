#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param is_data PARAM_DESCRIPTION
#' @param oos_data PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param pathaddin PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_stats
#' @export
fit_stats <- function(is_data,
                      oos_data,
                      indicator,
                      indicator_group,
                      run_date,
                      pathaddin) {

  ### Fit statistics
  model_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  image_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history")

  ## Append in- and out-of-sample coords
  is_data <- is_data[, c("latitude", "longitude", "period", "N", indicator), with = FALSE]
  is_data <- is_data[, oos := 0]
  oos_data <- oos_data[, c("latitude", "longitude", "period", "N", indicator), with = FALSE]
  oos_data <- oos_data[, oos := 1]
  all_data <- rbind(is_data, oos_data)
  if (indicator_family == "binomial") all_data <- all_data[, obs := get(indicator) / N]
  if (indicator_family != "binomial") all_data <- all_data[, obs := get(indicator)]

  ## Extract predictions at input data and bind to input datatable
  message("Loading cell_preds and inputs...")
  load(paste0(image_dir, "/", run_date, pathaddin, ".RData"))
  mean_preds <- brick(paste0(model_dir, "/", indicator, "_prediction_eb", pathaddin))
  load(paste0(model_dir, "/", indicator, "_cell_draws_eb", pathaddin, ".RData"))

  cell_pred.dt <- as.data.table(cell_pred)
  cols <- names(cell_pred.dt)
  message("Making upper and lower credible interval rasters...")
  cell_pred.dt <- cell_pred.dt[, upper := apply(.SD, 1, quantile, c(.975), na.rm = TRUE), .SDcols = cols]
  cell_pred.dt <- cell_pred.dt[, lower := apply(.SD, 1, quantile, c(.025), na.rm = TRUE), .SDcols = cols]
  upper <- cell_pred.dt[, upper]
  lower <- cell_pred.dt[, lower]

  upper_raster <- insertRaster(
    simple_raster,
    matrix(upper,
      ncol = length(unique(all_data$period))
    )
  )
  lower_raster <- insertRaster(
    simple_raster,
    matrix(lower,
      ncol = length(unique(all_data$period))
    )
  )

  message("Extracting upper and lower values at all coordinates...")
  # extract cluster covariates
  all_data$longitude <- as.numeric(all_data$longitude)
  all_data$latitude <- as.numeric(all_data$latitude)

  # add dummy column for time-varying covariates
  all_data$pred <- NA
  all_data$upper <- NA
  all_data$lower <- NA

  # loop through time varying predictions to insert them
  for (period in sort(unique(all_data$period))) {

    # find matching rows
    message(paste0("Extracting predictions for period ", period))
    idx_tv <- which(all_data$period == period)

    # get period prediction raster
    craster <- mean_preds[[period]]
    crs(craster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    uraster <- upper_raster[[period]]
    crs(uraster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    lraster <- lower_raster[[period]]
    crs(lraster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # extract values
    extr <- extract(craster, all_data[idx_tv, c("longitude", "latitude"), with = F])
    upper_extr <- extract(uraster, all_data[idx_tv, c("longitude", "latitude"), with = F])
    lower_extr <- extract(lraster, all_data[idx_tv, c("longitude", "latitude"), with = F])

    # add into df
    all_data[["pred"]][idx_tv] <- extr
    all_data[["upper"]][idx_tv] <- upper_extr
    all_data[["lower"]][idx_tv] <- lower_extr
  }

  ## Make fit statistics
  ## Add to df to combine later however we want
  message("Calculating all fit statistics at each coordinate...")

  # Drop missing predictions
  message(paste0("Predictions missing for ", length(all_data[is.na(pred), pred]), "/", length(all_data[, pred]), " observations"))
  message("If you have missings, this either due to missing covariate values (bad) or points that were in the buffer zone (fine).")
  message("Dropping missing rows...")
  df_no_nas <- all_data[!is.na(pred), ]

  # Coverage
  df_no_nas <- df_no_nas[obs >= lower & obs <= upper, covered := 1]
  df_no_nas <- df_no_nas[obs < lower | obs > upper, covered := 0]

  # Error
  df_no_nas <- df_no_nas[, error := obs - pred]

  # Make statistics over entire IS dataset
  mean_error <- mean(df_no_nas[oos == 0, error])
  mean_absolute_error <- mean(df_no_nas[oos == 0, abs(error)])
  rmse <- sqrt(mean(df_no_nas[oos == 0, error]^2))
  coverage <- mean(df_no_nas[oos == 0, covered])

  # Make statistics over entire OOS dataset
  oos_mean_error <- mean(df_no_nas[oos == 1, error])
  oos_mean_absolute_error <- mean(df_no_nas[oos == 1, abs(error)])
  oos_rmse <- sqrt(mean(df_no_nas[oos == 1, error]^2))
  oos_coverage <- mean(df_no_nas[oos == 1, covered])

  # Average over all observations
  message("IN-SAMPLE:")
  message(paste0("                       Average error:    ", round(mean_error, 3)))
  message(paste0("                       Average MAE:      ", round(mean_absolute_error, 3)))
  message(paste0("                       Average RMSE:     ", round(rmse, 3)))
  message(paste0("                       Average coverage: ", round(coverage, 3)))

  message("OUT-OF-SAMPLE:")
  message(paste0("                       Average error:    ", round(oos_mean_error, 3)))
  message(paste0("                       Average MAE:      ", round(oos_mean_absolute_error, 3)))
  message(paste0("                       Average RMSE:     ", round(oos_rmse, 3)))
  message(paste0("                       Average coverage: ", round(oos_coverage, 3)))

  fit_folder <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/fit_stats")
  message(paste0("Saving fit statistics in ", fit_folder))
  dir.create(fit_folder, showWarnings = FALSE)
  message(pathaddin)
  write.csv(df_no_nas, file = paste0(fit_folder, "/fit_stats_", pathaddin, ".csv"))
}
