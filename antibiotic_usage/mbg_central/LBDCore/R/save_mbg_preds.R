#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param config PARAM_DESCRIPTION
#' @param time_stamp PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param mean_ras PARAM_DESCRIPTION
#' @param sd_ras PARAM_DESCRIPTION
#' @param res_fit PARAM_DESCRIPTION
#' @param cell_pred PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param pathaddin PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname save_mbg_preds
#' @export
save_mbg_preds <- function(config, time_stamp, run_date, mean_ras, sd_ras, res_fit, cell_pred, df, pathaddin = "") {
  if (time_stamp == TRUE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  if (time_stamp == FALSE) output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/scratch")
  dir.create(output_dir, showWarnings = FALSE)

  # Save log of config file
  write.csv(config, paste0(output_dir, "/config.csv"), row.names = FALSE)

  if (!is.null(mean_ras)) {
    writeRaster(
      mean_ras,
      file = (paste0(output_dir, "/", indicator, "_prediction_eb", pathaddin)),
      overwrite = TRUE
    )
  }

  if (!is.null(sd_ras)) {
    # latent sd
    writeRaster(
      sd_ras,
      file = (paste0(output_dir, "/", indicator, "_sd_eb", pathaddin)),
      overwrite = TRUE
    )
  }

  # save model
  save(res_fit,
    file = (paste0(output_dir, "/", indicator, "_model_eb", pathaddin, ".RData"))
  )
  # save draws (with compression) to recombine later
  save(
    cell_pred,
    file = (paste0(output_dir, "/", indicator, "_cell_draws_eb", pathaddin, ".RData")),
    compress = TRUE
  )
  # save training data
  write.csv(
    df,
    file = (paste0(output_dir, "/", indicator, "_trainingdata", pathaddin, ".csv")),
    row.names = FALSE
  )

  # Write a an empty file to indicate done with this parallel script
  write(NULL, file = paste0(output_dir, "/fin_", pathaddin))
}
