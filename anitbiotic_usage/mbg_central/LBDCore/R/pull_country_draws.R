
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param periods PARAM_DESCRIPTION
#' @param raked PARAM_DESCRIPTION, Default: ''
#' @param in_dir PARAM_DESCRIPTION
#' @param pop_measure PARAM_DESCRIPTION
#' @param start_year PARAM_DESCRIPTION
#' @param end_year PARAM_DESCRIPTION
#' @param admin2_shapes PARAM_DESCRIPTION
#' @param all_region_pops PARAM_DESCRIPTION
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
#'  \code{\link[seegSDM]{character(0)}}
#'  \code{\link[raster]{extract}}
#' @rdname pull_country_draws
#' @export
pull_country_draws <- function(reg, periods, raked = "", in_dir,
                               pop_measure, start_year, end_year,
                               admin2_shapes, all_region_pops,
                               shapefile_version = "current") {
  ## updated and moved to misc_functions.R Sept 2018
  ## get_output_regions <- function(in_dir) {
  ##   regions <- list.files(in_dir, pattern = paste0(indicator, '_cell_draws_eb_'))
  ##   regions <- strsplit(regions, '_')
  ##   regions <- unlist(lapply(1:length(regions), parse_region_name, regions = regions))
  ##   regions <- unique(regions)
  ##   return(regions)
  ## }
  ## Pull draws by country and save template rasters by country in memory

  ## Load simple_raster and pop_raster into memory for this GAUL_CODE
  gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list,
    buffer = 0.4,
    subset_only = TRUE,
    shapefile_version = shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]
  raster_list <- build_simple_raster_pop(subset_shape)
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]

  ## Load draws into memory for this GAUL_CODE.
  message(paste0("Loading draws for ", reg, "..."))
  load(paste0(in_dir, "/", indicator, raked, "_cell_draws_eb_bin0_", reg, "_0.RData"))
  draws <- as.data.table(cell_pred)
  rm(cell_pred)

  ## Get dt or draws for this region indexed by GAUL_CODE.
  cell_idx <- notMissingIdx(simple_raster)
  coords <- xyFromCell(simple_raster, notMissingIdx(simple_raster))
  template <- raster::extract(simple_raster, coords)
  draws_by_gaul <- draws[, GAUL_CODE := rep(template, periods)]

  ## Pull out draws/templates for each GAUL_CODE in this region.
  message("Pulling draws by country for ", reg, "...")

  reg_gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
  reg_gaul_list <- reg_gaul_list[!(reg_gaul_list %in% 40762)]
  return_list <- lapply(reg_gaul_list, make_country_list,
    reg_simple_raster = simple_raster
  )
  names(return_list) <- paste0("list_", reg_gaul_list)

  ## Return list of draws, pops, templates
  return(return_list)
}
