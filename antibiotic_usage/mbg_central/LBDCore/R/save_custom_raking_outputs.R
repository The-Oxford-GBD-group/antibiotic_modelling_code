#' @title Save outputs from custom raking function
#' @description This function saves outputs from \code{custom_rake}
#'
#' @param custom_rake_output Output list from custom raking function
#' @param outdir Directory for files to be saved to
#' @param indicator Name of indicator being modeled
#' @param age_group Name of age group
#' @param prefix Character string to be added to files to avoid overwriting non-custom files in folder
#' @param reg Name of region that was modeled
#'
#' @return NULL
#' @export
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
save_custom_raking_outputs <- function(custom_rake_output,
                                       outdir,
                                       indicator,
                                       prefix,
                                       reg) {
  ol <- custom_rake_output
  raked_cell_pred <- ol[["raked_cell_pred"]]
  save(raked_cell_pred, file = sprintf("%s/%s_%s_raked_cell_draws_eb_bin0_%s_0.RData", outdir, prefix, indicator, reg))
  simple_raster <- ol[["simple_raster"]]
  writeRaster(simple_raster, file = sprintf("%s/%s_%s_simple_raster", outdir, prefix, indicator), format = "GTiff")
  raking_factors <- ol[["raking_factors"]]
  write.csv(raking_factors, file = sprintf("%s/%s_%s_%s_rf.csv", outdir, prefix, indicator, reg))
  adm0_geo <- ol[["adm0_geo"]]
  write.csv(adm0_geo, file = sprintf("%s/%s_%s_adm0_geo.csv", outdir, prefix, indicator))
  mean_raster <- ol[["mean_raked_raster"]]
  writeRaster(mean_raster, file = sprintf("%s/%s_%s_mean_raked_2000_2015", outdir, prefix, indicator), format = "GTiff")
  cirange_raster <- ol[["cirange_raked_raster"]]
  writeRaster(cirange_raster, file = sprintf("%s/%s_%s_cirange_raked_2000_2015", outdir, prefix, indicator), format = "GTiff")
  upper_raster <- ol[["upper_raked_raster"]]
  writeRaster(upper_raster, file = sprintf("%s/%s_%s_upper_raked_2000_2015", outdir, prefix, indicator), format = "GTiff")
  lower_raster <- ol[["lower_raked_raster"]]
  writeRaster(lower_raster, file = sprintf("%s/%s_%s_lower_raked_2000_2015", outdir, prefix, indicator), format = "GTiff")
}
