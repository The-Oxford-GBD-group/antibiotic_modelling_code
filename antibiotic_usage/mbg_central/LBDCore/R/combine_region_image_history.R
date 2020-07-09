#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param fixed_effects PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{merge}}
#' @rdname combine_region_image_history
#' @export
#' @importFrom raster merge
combine_region_image_history <- function(indicator, indicator_group, run_date, fixed_effects) {

  ## Combine input data and covariates layers for models run by region
  ## Necessary for saving to Shiny tool
  ## Save "covs", "tv_*", and "df" to new combined snapshot in model_image_history

  # # Combine non-varying covs
  # load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, '_bin0_wssa_0.RData'))
  # nt_covs <- names(cov_list)[!grepl("gaul_code", names(cov_list))]
  # combine_nt_cov <- function(nt_cov) {
  #   pull_nt_covs <- function(region) {
  #     load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, '_bin0_', region, '_0.RData'))
  #     cov_layer <- cov_list[[nt_cov]]
  #     return(cov_layer)
  #   }
  #   region_layers <- lapply(Regions, pull_nt_covs)
  #   combined_layers <- do.call(raster::merge, region_layers)
  #   names(combined_layers) <- nt_cov
  #   return(combined_layers)
  # }
  # cov_list <- lapply(nt_covs, combine_nt_cov)
  # cov_list <- do.call(raster::brick, cov_list)
  #
  # # Combine varying covs
  # selected_covs <- strsplit(fixed_effects," ")
  # selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]
  # for(c in selected_covs) {
  #   if(paste0('tv_',c) %in% grep('tv_*', ls(), value = TRUE)) {
  #     pull_tv_covs <- function(region) {
  #       load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, '_bin0_', region, '_0.RData'))
  #       tv_cov <- get(paste0('tv_',c))
  #       return(tv_cov)
  #     }
  #     region_layers <- lapply(Regions, pull_tv_covs)
  #     combined_layers <- do.call(raster::merge, region_layers)
  #     names(combined_layers) <- gsub("layer", c, names(combined_layers))
  #     assign(paste0('tv_', c), combined_layers)
  #   }
  # }

  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, "_bin0_wssa_0.RData"))
  new_cov_list <- list()
  for (cov in names(cov_list)[!grepl("gaul_code", names(cov_list))]) {
    pull_raster_covs <- function(region) {
      load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, "_bin0_", region, "_0.RData"))
      cov_raster <- cov_list[[cov]]
      return(cov_raster)
    }
    region_layers <- lapply(Regions, pull_raster_covs)
    combined_layers <- do.call(raster::merge, region_layers)
    names(combined_layers) <- gsub("layer", cov, names(combined_layers))
    if (length(names(combined_layers)) == 1) new_cov_list[[cov]] <- combined_layers
    if (length(names(combined_layers)) != 1) assign(paste0("tv_", cov), combined_layers)
  }
  # cov_list <- do.call(raster::brick, new_cov_list)
  cov_list <- brick(new_cov_list)

  # Combine input data
  pull_df <- function(region) {
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, "_bin0_", region, "_0.RData"))
    return(df)
  }
  df <- lapply(Regions, pull_df)
  df <- do.call(rbind.fill, df)

  covs <- cov_list

  save(list = c("df", "covs", grep("^tv_*", ls(), value = TRUE)), file = paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, ".RData"))
}
