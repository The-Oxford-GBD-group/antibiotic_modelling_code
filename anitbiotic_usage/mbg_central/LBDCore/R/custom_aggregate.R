#' @title Custom Aggregation Function
#' @description Create custom aggregates
#' @param cell_pred Cell pred object to be aggregated
#' @param custom_shapefile_path Path to shapefile that will be used for aggregation
#' @param custom_shapefile Alternatively can specify a shapefile object directly
#' @param field Field in shapefile that should be used for aggregation (ADM2_CODE, etc).
#' @param reg Region used to produce cell pred object.
#' @param yl List of years
#' @param ss Summary statistics
#' @param return_shapefile Boolean, should the function return the shapefile?
#' @param verbose Should the function be talkative?
#' @param modeling_shapefile_version string used to specify which shapefile version to use for merging adm codes to pixels. should be the version used to generate the cell_preds
#'
#' @return Returns a list containing:
#'          \code{draws}: data.table with rows for each aggregated area & year,
#'                   and columns representing [field], year, and draws[V1...Vn]
#'          \code{shapefile_data}: contents of the shapefile defined by \code{shapefile_path},
#'                            which can be merged on to "draws" to add on admin names, etc
#'          \code{summarized}: data table with row for each aggregated area & year,
#'                        and columns for [field], year, and all summary stats in \code{ss}
#'                        (if \code{ss}=NULL, then this is also NULL)
#'          \code{shapefile}: the shapefile itself (if return_shapefile = T; otherwise will be NULL)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' custom_agg_output <- custom_aggregate(
#'   cell_pred = cell_pred,
#'   shapefile_path = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/ETH_adm3_2007.shp",
#'   field = "ID_3",
#'   reg = "essa",
#'   yl = c(2000:2016),
#'   pop_meas = pop_measure,
#'   ss = c("mean", "upper", "lower", "cirange"),
#'   verbose = T
#' )
#' }
custom_aggregate <- function(cell_pred,
                             shapefile_path,
                             field,
                             reg,
                             yl,
                             pop_meas = pop_measure,
                             ss = NULL,
                             return_shapefile = T,
                             verbose = T,
                             modeling_shapefile_version = "current") {

  # initialize results list
  outputlist <- list()

  ## get simple polygon and simple raster used to produce cell pred
  if (verbose) message("Loading simple polygon")
  eval_adm0_codes <- get_adm0_codes(reg, shapefile_version = modeling_shapefile_version)
  simple_polygon <- load_simple_polygon(
    gaul_list = eval_adm0_codes,
    buffer = 0.4, shapefile_version = modeling_shapefile_version
  )
  subset_shape <- simple_polygon[["subset_shape"]]
  simple_polygon <- simple_polygon[["spoly_spdf"]]

  if (verbose) message("Loading simple raster")
  raster_list <- build_simple_raster_pop(subset_shape)
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]

  # get new simple polygon and simple raster that will be raked to
  if (verbose) message("Loading new simple polygon to be raked to")
  new_simple_polygon <- load_simple_polygon(gaul_list = get_adm0_codes(reg), buffer = 0.4, custom_shapefile_path = shapefile_path)
  new_subset_shape <- new_simple_polygon[["subset_shape"]]
  new_simple_polygon <- new_simple_polygon[["spoly_spdf"]]

  if (verbose) message("Loading new simple raster to be raked to")
  new_raster_list <- build_simple_raster_pop(new_subset_shape, field = field)
  new_simple_raster <- new_raster_list[["simple_raster"]]
  new_pop_raster <- new_raster_list[["pop_raster"]]

  # get extents of original and simple raster to line up - extend and crop just in case
  new_simple_raster <- extend(new_simple_raster, simple_raster, values = NA)
  new_simple_raster <- crop(new_simple_raster, extent(simple_raster))

  # crosswalk cell_pred to new raster and rename outputs
  if (verbose) message("Crosswalking cell pred object to new raster")
  new_pred_object <- crosswalk_cell_pred_add_NA(simple_raster, new_simple_raster, cell_pred, length(yl))
  cell_pred <- new_pred_object[[1]]
  simple_raster <- new_pred_object[[2]]

  # if interval month doesn't exist, use year list to determine 5-year or annual
  if (!exists("interval_mo")) {
    if ((yl[[2]] - yl[[1]]) %% 5 == 0) {
      interval_mo <- 60
    } else {
      interval_mo <- 12
    }
  }

  if (verbose) message("Getting population raster")
  ## Pull annual population brick using new covariates function
  if (class(yl) == "character") yl <- eval(parse(text = yl))
  pop_raster_annual <- load_and_crop_covariates_annual(
    covs = "worldpop",
    measures = pop_meas,
    simple_polygon = simple_polygon,
    start_year = min(yl),
    end_year = max(yl),
    interval_mo = as.numeric(interval_mo),
    agebin = 1
  )[[1]]

  ## TO DO: test if this works for other shapefiles, might need extend?
  pop_raster_annual <- crop(pop_raster_annual, extent(simple_raster))
  pop_raster_annual <- setExtent(pop_raster_annual, simple_raster)
  pop_raster_annual <- mask(pop_raster_annual, simple_raster)

  # get custom admin raster
  ## TO DO: check extents against simple raster
  custom_admin_raster <- load_custom_admin_raster(shapefile_path, field, simple_raster)

  ## Create population weights using the annual brick and feed custom year argument to aggregation function
  if (verbose) message("Building population weights object")
  pop_wts_adm0 <- make_population_weights(
    admin_level = 0,
    simple_raster = simple_raster,
    pop_raster = pop_raster_annual,
    gaul_list = get_adm0_codes(reg),
    custom_admin_raster = custom_admin_raster
  )

  if (verbose) message("Making condSim")
  cond_sim_draw_adm0 <- make_condSim(
    admin_level = 0,
    pop_wts_object = pop_wts_adm0,
    cell_pred = cell_pred,
    gaul_list = get_adm0_codes(reg),
    summarize = FALSE,
    years = yl
  )

  # Add ID and year on to this admin level draw object
  admin_draws <- data.table(split_geo_names(as.matrix(cond_sim_draw_adm0)), cond_sim_draw_adm0)
  admin_draws$year <- as.numeric(admin_draws$year)
  admin_draws$name <- as.numeric(admin_draws$name)
  setnames(admin_draws, "name", field)

  if (verbose) message("Storing admin draws")
  outputlist[["draws"]] <- admin_draws

  if (verbose) message("Storing shapefile information")

  if (!is.null(custom_shapefile_path)) the_shapefile <- readOGR(custom_shapefile_path)
  if (!is.null(custom_shapefile)) the_shapefile <- custom_shapefile

  outputlist[["shapefile_data"]] <- as.data.table(the_shapefile)

  # Calculate summary statistics
  if (!is.null(ss)) {
    if (verbose) message("Calculating and storing summary statistics")
    ss_df <- lapply(ss, function(summstat) {
      draw_cols <- names(admin_draws)[grepl("V[0-9]+", names(admin_draws))]
      out <- admin_draws[, .(stat = apply(.SD, 1, summstat)), by = list(get(field), year), .SDcols = draw_cols]
      setnames(out, c("stat", "get"), c(summstat, field))
      return(out)
    })

    outputlist[["summarized"]] <- Reduce(merge, ss_df)
  } else {
    outputlist[["summarized"]] <- NULL
  }

  if (return_shapefile == T) {
    outputlist[["shapefile"]] <- the_shapefile
  } else {
    outputlist[["shapefile"]] <- NULL
  }

  return(outputlist)
}
