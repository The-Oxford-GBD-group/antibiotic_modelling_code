#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_group PARAM_DESCRIPTION, Default: indicator_group
#' @param df PARAM_DESCRIPTION, Default: df
#' @param simple_raster PARAM_DESCRIPTION, Default: simple_raster
#' @param mesh_s PARAM_DESCRIPTION, Default: mesh_s
#' @param mesh_t PARAM_DESCRIPTION, Default: mesh_t
#' @param cov_list PARAM_DESCRIPTION, Default: all_cov_layers
#' @param run_date PARAM_DESCRIPTION, Default: NULL
#' @param pathaddin PARAM_DESCRIPTION, Default: ''
#' @param child_model_names PARAM_DESCRIPTION, Default: NULL
#' @param all_fixed_effects PARAM_DESCRIPTION, Default: NULL
#' @param period_map PARAM_DESCRIPTION, Default: NULL
#' @param centre_scale PARAM_DESCRIPTION, Default: T
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
#' @rdname save_mbg_input
#' @export
#' @importFrom raster mask
save_mbg_input <- function(indicator = indicator, 
                           indicator_group = indicator_group, 
                           df = df, 
                           simple_raster = simple_raster,
                           simple_raster2 = NULL,
                           mesh_s = mesh_s,
                           mesh_t = mesh_t, 
                           cov_list = all_cov_layers, 
                           run_date = NULL, 
                           pathaddin = "",
                           child_model_names = NULL, 
                           all_fixed_effects = NULL, 
                           period_map = NULL, centre_scale = T) {
  period_map <- copy(period_map) ## to avoid global scoping issues
  just_covs <- extract_covariates(df, cov_list, return_only_results = T, 
                                  centre_scale = centre_scale, 
                                  period_var = "year", period_map = period_map)
  if (centre_scale == TRUE) {
    just_covs <- just_covs$covs
  }
  just_covs <- just_covs[, year := NULL]
  just_covs <- just_covs[, period_id := NULL]
  df <- cbind(df, just_covs)

  # create a period column
  if (is.null(period_map)) {
    period_map <- make_period_map(c(2000, 2005, 2010, 2015))
  }
  df[, period := NULL]
  setnames(period_map, "data_period", "year")
  setnames(period_map, "period_id", "period")
  df <- merge(df, period_map, by = "year", sort = F)

  ## Now that we've extracted covariate values to our data in the buffer zone,
  ## clip cov list to simple_raster instead of simple_polygon
  ##    (simple_raster is area we actually want to model over)
  for (l in 1:length(cov_list)) {
    cov_list[[l]] <- crop(cov_list[[l]], extent(simple_raster))
    cov_list[[l]] <- setExtent(cov_list[[l]], simple_raster)
    cov_list[[l]] <- raster::mask(cov_list[[l]], simple_raster)
  }

  ## Save all inputs
  to_save <- c("df", "simple_raster", "mesh_s", "mesh_t", "cov_list", "child_model_names", "all_fixed_effects", "period_map")
  if (!is.null(simple_raster2)) to_save <- c(to_save, "simple_raster2")
  save(list = to_save, file = paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, pathaddin, ".RData"))
  message(paste0("All inputs saved to /share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, pathaddin, ".RData"))
}
