## these functions are being written specifically to help get the
## Diarrhea #1 paper published, but they should be more generally
## useful
## #######################################################################
## they take cell pred type objects and make analagous (matrix where
## rows are observations at some level and columns are wide on draws)
## annual-rate-of-change_pred (AROC_pred) type objects
## these functions then also perform projections on make analgous
## projection_pred objects
## written by aoz / modified by chris troeger, then jon mosser
## #######################################################################
## get_cell_pred_for_aroc ################################################
#' @title Pull cell pred object and format for aroc or projection calculations
#' @description A helper function that pulls a cell pred object and then formats it for use in other functions
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param reg region
#' @param measure prevalence, incidence, mortality, etc
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_\%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_\%s_0.RData', reg)
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param rk raked? (logical)
#' @param shapefile_version string which shapefile version to use for indexing cell_preds
#'
#' @return a formated \code{cell_pred} object
#'
#' @export
get_cell_pred_for_aroc <- function(ind_gp,
                                   ind,
                                   rd,
                                   reg,
                                   measure,
                                   matrix_pred_name = NULL,
                                   skip_cols = NULL,
                                   rk = T,
                                   shapefile_version = "current") {
  # Load the relevant pred object - loads an object named cell_pred
  # Try both rds file and rdata file until we standardize this
  rds_file <- sprintf(paste0(
    "/share/geospatial/mbg/%s/%s/output/%s/",
    matrix_pred_name
  ), ind_gp, ind, rd, reg, measure)

  if (rk) {
    rdata_file <- paste0(
      "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/",
      ind, "_raked_cell_draws_eb_bin0_", reg, "_0.RData"
    )
  } else {
    rdata_file <- paste0(
      "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/",
      ind, "_cell_draws_eb_bin0_", reg, "_0.RData"
    )
  }

  if (!is.null(matrix_pred_name) & file.exists(rds_file)) {
    cell_pred <- readRDS(rds_file)
  }

  if (rk) {
    if (file.exists(rdata_file)) {
      load(rdata_file)
      cell_pred <- raked_cell_pred
      rm(raked_cell_pred)
    }
  } else {
    if (file.exists(rdata_file)) {
      load(rdata_file)
    }
  }

  # Check to make sure loaded correctly
  if (!exists("cell_pred")) stop("Unable to load raked cell pred object!")

  # If extra columns at front of cell_pred, can skip here
  if (!(is.null(skip_cols))) cell_pred <- as.matrix(cell_pred[, (skip_cols + 1):ncol(cell_pred)])

  ## then we need to load the simple raster to determine the
  ## indexing between pixel-years, and matrix rows
  message("-- making simple raster")
  gaul_list <- get_adm0_codes(reg, shapefile_version = shapefile_version)
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list, buffer = 0.4, subset_only = T,
    shapefile_version = shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  raster_list <- build_simple_raster_pop(subset_shape)
  simple_raster <- raster_list[["simple_raster"]] ## this is what we really need

  ## then we add two columns to the cell_preds. an spatial index
  ## and a year col
  cell_idx <- notMissingIdx(simple_raster)
  num_yrs <- nrow(cell_pred) / length(cell_idx)
  ## this should be an integer year. if it isn't, something isn't
  ## synced correctly between preds and simple_rasters
  if (round(num_yrs, 0) != num_yrs) {
    stop(paste0(
      "Error! The number of rows in the matrix_pred object for region %s is not divisible by ",
      "the number of modeling cells in the associated simple raster object that was loaded for ",
      "the region. Some dimension mismatch has occured and needs to be investigated in region: ",
      reg
    ))
  }

  new_cols <- cbind(
    rep(1:num_yrs, each = length(cell_idx)),
    rep(cell_idx, num_yrs)
  )
  colnames(new_cols) <- c("year", "idx")
  cell_pred <- cbind(new_cols, cell_pred)

  return(cell_pred)
}
