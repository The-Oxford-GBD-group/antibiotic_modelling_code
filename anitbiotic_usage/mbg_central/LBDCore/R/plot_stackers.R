#' @title Plot Stackers
#' @description Plot maps of stackers, mean raster of covariate, and data informing model
#'
#' @param reg region
#' @param ig indicator group
#' @param ind indicator
#' @param rd run date
#' @param ss vector of stacker names
#' @param yl year list (in vector form, e.g. `c(2000:2015)`)
#' @param zmin minimum value for color scheme (if NULL will calculate from data)
#' @param zmax maximum value for color scheme (if NULL will calculate from data)
#' @param sh_dir `/share` directory, including run date
#' @param highisbad should high values be colored in red ("bad")? Logical.
#' @param o_dir output directory
#' @param individual_countries should individual countries be graphed as well? Logical.
#' @param shapefile_version string specifies shapefile version to be used
#' @return Writes a series of image fileswith maps of each stacker, mean covariate raster, and
#'         a map of input data for each year-region combination (and year-country if individual_countries = T)
#'         in prespecified folder structure
#' @examples
#' \dontrun{
#' mclapply(Regions, function(r) {
#'   message(paste0("Making stacker maps for region: ", r))
#'   plot_stackers(reg = r, highisbad = F, individual_countries = T)
#' }, mc.cores = 5)
#' }
#' @export
plot_stackers <- function(reg,
                          ig = indicator_group,
                          ind = indicator,
                          rd = run_date,
                          ss = stackers,
                          yl = year_list,
                          zmin = NULL, zmax = NULL,
                          sh_dir = sharedir,
                          highisbad = F,
                          o_dir = out_dir,
                          individual_countries = T,
                          shapefile_version = "current") {

  # Load master shape for outlines
  # master_shape <- readRDS('/share/geospatial/rds_shapefiles/gdcv_custom/master_shape_all.rds')
  master_shape <- readRDS(get_admin_shapefile(admin_level = 0, version = shapefile_version, suffix = ".rds"))
  master_shape <- subset(master_shape, ADM0_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version))

  # Set up output dir
  o_dir <- paste0(o_dir, "/stacker_maps/")
  dir.create(o_dir, recursive = T, showWarnings = F)

  # Load stacker objects
  load(paste0(
    "/share/geospatial/mbg/", ig, "/", ind, "/model_image_history/",
    rd, "_bin0_", reg, "_0.RData"
  ))
  # subset cov_list to stackers
  stacker_list <- cov_list[which(names(cov_list) %in% ss)]


  # load mean unraked raster for estimates. Try region rasters first; whole raster & crop if not.
  if (file.exists(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else if (file.exists(paste0(sh_dir, ind, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else {
    stop("Could not find unraked raster .tif.")
  }

  # add mean raster to stacker list
  stacker_list[[ind]] <- result_brick

  # load input data from csv if present; re-generate if needed
  if (file.exists(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"))) {
    input_df <- read.csv(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"),
      stringsAsFactors = F
    ) %>%
      as.data.table()
  } else {
    gaul_list <- get_adm0_codes(reg,
      shapefile_version = shapefile_version
    )
    simple_polygon_list <- load_simple_polygon(
      gaul_list = gaul_list,
      buffer = 0.4,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    raster_list <- build_simple_raster_pop(subset_shape)
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]

    input_df <- load_input_data(
      indicator = indicator,
      simple = simple_polygon,
      removeyemen = TRUE,
      yl = yl
    )
  }

  input_df[, outcome := get(indicator) / N]


  # Save map for entire region
  save_maps(input_df, stacker_list, master_shape, result_brick, zmin,
    zmax, yl, ind, ig, sh_dir, highisbad, o_dir,
    ctry = NULL,
    shapefile_version = shapefile_version
  )

  # Save maps for individual countries
  if (individual_countries == T) {
    gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
    gaul_list <- data.table(GAUL_CODE = get_adm0_codes(reg, shapefile_version = shapefile_version))
    gaul_list <- merge(gaul_list, gaul_to_loc_id, by = "GAUL_CODE")
    for (c in unique(gaul_list$ihme_lc_id)) {
      if (!(get_adm0_codes(c, shapefile_version = shapefile_version) %in% master_shape$ADM0_CODE)) {
        message(paste0("No shapes in master_shape corresponding to admin 0 code for ", c, " - skipping..."))
      } else {
        message(paste0("Saving stacker maps for country: ", c))
        save_maps(input_df, stacker_list, master_shape, result_brick, zmin, zmax, yl, ind, ig, sh_dir, highisbad, o_dir, ctry = c, shapefile_version = shapefile_version)
      }
    }
  }
}
