#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param measure PARAM_DESCRIPTION, Default: 'mortality'
#' @param baseline_year PARAM_DESCRIPTION, Default: 2000
#' @param year_to_map PARAM_DESCRIPTION, Default: 2015
#' @param goal_threshold PARAM_DESCRIPTION, Default: 0.001
#' @param metric PARAM_DESCRIPTION, Default: 'sdgprob'
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
#'  \code{\link[raster]{merge}}
#' @rdname compile_results_table
#' @export
#' @importFrom raster merge
compile_results_table <- function(indicator_group,
                                  indicator,
                                  run_date,
                                  measure = "mortality",
                                  baseline_year = 2000,
                                  year_to_map = 2015,
                                  goal_threshold = 0.001,
                                  metric = "sdgprob",
                                  shapefile_version = "current") {
  results_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/table_", baseline_year)
  dir.create(paste0(results_dir, "/summary_tables"))
  all_files <- list.files(results_dir, pattern = measure, full.names = TRUE)
  if (metric == "percent") all_files <- all_files[grepl("_percent2010", all_files)]
  if (metric == "sdgprob") all_files <- all_files[!grepl("_percent2010", all_files)]
  all_admins <- rbindlist(lapply(all_files, fread))
  all_admins <- all_admins[is.na(mean), mean := goal_threshold]
  all_admins <- all_admins[is.na(upper), upper := goal_threshold]
  all_admins <- all_admins[is.na(lower), lower := goal_threshold]
  setnames(all_admins, "admin2", "ADM2_CODE")
  setnames(all_admins, "admin1", "ADM1_CODE")
  setnames(all_admins, "admin0", "ADM0_CODE")
  all_gauls <- unique(all_admins[, ADM0_CODE])
  for (admin_level in c(0, 1, 2)) {
    admin_names <- read.dbf(get_admin_shapefile(admin_level,
      suffix = ".dbf",
      version = shapefile_version
    ))
    admin_names <- as.data.table(admin_names)
    admin_names <- admin_names[, c(paste0("ADM", admin_level, "_CODE"), paste0("ADM", admin_level, "_NAME")), with = FALSE]
    all_admins <- merge(all_admins, admin_names, by = paste0("ADM", admin_level, "_CODE"), all.x = TRUE)
  }

  write.csv(
    all_admins,
    paste0(results_dir, "/summary_tables/", indicator, "_", measure, "_", metric, "_summary_table.csv")
  )

  ## Save copy of all_admins (by level) for Lucas
  for (adm in c(0, 1, 2)) {
    if (adm == 0) {
      this_admin_data <- all_admins[is.na(ADM1_CODE) & is.na(ADM2_CODE), ]
      this_admin_data <- this_admin_data[year == year_to_map, ]
    }
    if (adm == 1) {
      this_admin_data <- all_admins[!is.na(ADM1_CODE) & is.na(ADM2_CODE), ]
      this_admin_data <- this_admin_data[year == year_to_map, ]
    }
    if (adm == 2) {
      this_admin_data <- all_admins[!is.na(ADM1_CODE) & !is.na(ADM2_CODE), ]
      this_admin_data <- this_admin_data[year == year_to_map, ]
    }
    write.csv(this_admin_data, paste0(results_dir, "/pixels/adm", adm, "_", indicator, "_", measure, "_", metric, year_to_map, "_summary_table.csv"))
  }

  make_africa_raster <- function(gaul) {

    # Convert raster to SpatialPointsDataFrame
    message(paste0("rasterizing ", gaul, "..."))
    pixel_probs <- fread(paste0(results_dir, "/pixels/", measure, "_", gaul, "_probs.csv"))
    simple_raster <- raster(paste0(results_dir, "/simple/", gaul, ".tif"))
    probs_raster <- insertRaster(simple_raster, matrix(pixel_probs[, p_goal], ncol = 1))
    return(probs_raster)
  }

  africa_raster <- lapply(unique(all_admins[, ADM0_CODE]), make_africa_raster)
  final_africa_raster <- do.call(raster::merge, africa_raster)
  writeRaster(final_africa_raster,
    file = paste0(results_dir, "/pixels/", indicator, "_", measure, "_", metric, year_to_map),
    format = "GTiff",
    overwrite = TRUE
  )
}
