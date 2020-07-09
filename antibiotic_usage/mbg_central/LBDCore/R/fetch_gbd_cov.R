#' @title Fetch GBD covariates
#' @description Get GBD covariates
#' @param name Name of covariate
#' @param measure Measure of covariate
#' @param age_ids Age group IDs
#' @param year_ids Year IDs
#' @param afras PARAM_DESCRIPTION
#' @param shapefile_version Shapefile version
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{getValues}}
#' @rdname fetch_gbd_cov
#' @export
#' @importFrom raster getValues
fetch_gbd_cov <- function(name, measure, age_ids, afras, year_ids, shapefile_version) {

  # Load country-level results
  message("  Loading: ", name)
  gbd <- load_gbd_data(
    gbd_type = measure,
    gbd_name = name,
    gaul_list = unique(afras),
    measure_id = 5,
    age_group_id = age_ids,
    metric_id = 3,
    year_ids = year_ids,
    return_by_age_sex = "no",
    shapefile_version = shapefile_version, ## should be most up-to-date modified GAUL
    ## to match GBD2016_analysis_final.rds
    ## world shapefile
    collapse_age_sex = TRUE
  )

  if (nrow(gbd) != nrow(unique(gbd[, list(name, year)]))) stop(paste0(name, "is not unique by location-year"))

  # For each gaul code and year, update the values
  blank <- brick(lapply(year_ids, function(x) afras * NA))

  for (yyy in 1:length(year_ids)) {
    for (ggg in unique(afras)) {
      blank[[yyy]][which(raster::getValues(afras) == ggg)] <- gbd[name == ggg & year == year_ids[yyy], mean]
    }
  }

  names(blank) <- rep(name, times = dim(blank)[3])

  return(blank)
}
