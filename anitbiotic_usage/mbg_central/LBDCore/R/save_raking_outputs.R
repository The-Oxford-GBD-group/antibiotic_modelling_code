#' @title Save Outputs from Raking Function
#'
#' @param rake_output Output list from subnational raking function
#' @param outdir character string - Directory for files to be saved to
#' @param indicator charater string - Name of indicator being modeled
#' @param age_group character string - Name of age group
#' @param prefix Character string to be added to start of file name to avoid overwriting non-custom files in folder
#' @param reg character string - Name of region that was modeled
#' @param year_list integer vector of years
#' @param save_vec default `NULL`. If null, uses names from subnational_rake_output. Otherwise takes a character vector with the names of objects in subnational_rake_output to save.
#' @param save_draws_as character string default `RDS`. additional supported arguments `rds` and `Rdata`. The file type to save raked draws. If using `RDS` or `rds`, the file extension will be .RDS or .rds accordingly.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' save_custom_raking_outputs(custom_rake_output,
#'   outdir = sprintf("/share/geospatial/mbg/u5m/%s_%s/output/%s", indicator, age_group, run_date),
#'   indicator,
#'   age_group,
#'   prefix = "custom_india",
#'   reg = "south_asia"
#' )
#' }
#' @export
save_raking_outputs <- function(rake_output,
                                outdir,
                                indicator,
                                prefix,
                                reg,
                                year_list,
                                save_vec = NULL,
                                save_draws_as = "RDS") {
  ol <- rake_output

  if (is.null(save_vec)) {
    save_vec <- names(ol)
  }

  if (prefix != "") {
    prefix <- paste0(prefix, "_")
  }

  if ("raked_cell_pred" %in% save_vec) {
    raked_cell_pred <- ol[["raked_cell_pred"]]
    if (save_draws_as %in% c("rds", "RDS")) {
      saveRDS(raked_cell_pred, file = sprintf("%s/%s%s_raked_cell_draws_eb_bin0_%s_0.%s", outdir, prefix, indicator, reg, save_draws_as))
    } else if (save_draws_as == "RData") {
      save(raked_cell_pred, file = sprintf("%s/%s%s_raked_cell_draws_eb_bin0_%s_0.RData", outdir, prefix, indicator, reg))
    }
  }
  if ("new_simple_raster" %in% save_vec) {
    simple_raster <- ol[["new_simple_raster"]]
    writeRaster(simple_raster, file = sprintf("%s/%s%s_%s_simple_raster", outdir, prefix, indicator, reg), format = "GTiff", overwrite = T)
  }
  if ("raking_factors" %in% save_vec) {
    raking_factors <- ol[["raking_factors"]]
    write.csv(raking_factors, file = sprintf("%s/%s%s_%s_rf.csv", outdir, prefix, indicator, reg))
  }

  # get all rasters in save vec
  rasters <- ol[grep("raster$", names(ol))]
  # remove simple raster, which is saved separately
  rasters <- rasters[names(rasters) != "new_simple_raster"]
  # loop through rasters and save them all using their list names
  for (ras in names(rasters)) {
    if (ras %in% save_vec) {
      ras_name <- substring(ras, 1, nchar(ras) - 7)
      writeRaster(rasters[[ras]], file = sprintf("%s/%s%s_%s_%s_%i_%i", outdir, prefix, indicator, ras_name, reg, min(year_list), max(year_list)), format = "GTiff", overwrite = T)
    }
  }
}
