#' @title Standard Raking Function
#'
#' @author Michael Collison
#'
#' @description A function used to rake mbg ouputs to GBD estimates. Can rake to either national level or subnational level (where available). Supports linear and logit raking. Optionally outputs summary rasters of raked cell pred object.
#'
#' @param cell_pred Cell pred object to be raked.
#' @param rake_to Df with `name`, `year`, and `mean` columns. values in name must match up with values in `field`.
#' @param reg character string - Region used to produce cell pred object.
#' @param year_list integer vector - vector of years
#' @param pop_measure character string - population measure (can be found in config of model run)
#' @param rake_method character string - must be either `linear` or `logit`
#' @param rake_subnational boolean default `T`. If true, uses the subnational raking shapefile, otherwise uses the adm0 shapefile. Make sure that values in `rake_to` contain the codes for level of raking chosen.
#' @param crosswalk Boolean default `T`, for models run before the new gaul shapefiles ready on 7/6/18, the cell_pred needs to be crosswalked to match the subnational raking raster. Introduces NAs into raster where the new simple raster has values but the original does not.
#' @param shapefile_path character string -Path to shapefile that will be used for raking. Preset to subnational raking shapefile, don't change.
#' @param field character string - Field in shapefile that has admin identifiers that match with rake_to. Preset to ihme_loc_ids at the lowest level in the shapefile. Don't change.
#' @param zero_heuristic Boolean default `F`.  If logit raking, this will automatically set rf = -9999 for any country-year with a target value of 0.  This produces a raked value of 0 for all pixels.  Raking to a target of zero in logit space is very time-consuming and the algorithm
#'                       can only approach zero asymptotically.  For situations where the target is truly zero (most useful for, say, an intervention pre-introduction) this will both speed up the process and ensure that zeros are returned.
#' @param approx_0_1 Boolean default `F`. If logit raking, any values of zero will be replaced with 1e-10 and values of 1 will be replaced with (1-(1e-10)).  Otherwise, logit transformations will fail in `NewFindK`. Useful if some areas have very low or high predicted values in `cell_pred`,
#'                   such that some draws are either 0 or 1 (or extremely close to these values).
#' @param simple_raster default `NULL`, option to pass in simple raster to function if it's been loaded already. NOTE: if the pop raster is not being passed in as well, the simple_polygon needs to be supplied as well. There is a check to ensure this.
#' @param simple_polygon default `NULL`, option to pass in simple polygon if its been loaded already. This is necessary if the simple raster is being passed in, but the pop raster is not. The covariate loading function requires a simple polygon.
#' @param pop_raster default `NULL`, option to pass in pop raster if its been loaded already.
#'
#' Additional Parameters (...)
#' @param if_no_gbd default `return_na`, other option `return_unraked`. If return_na, any location-years without gbd estimates will return NA raking factors. if return_unraked, will any location-years without gbd estimates will return 1 for linear raking, 0 for logit raking.
#' @param MaxJump default `10`. Maximum size of a jump to the answer (for logit raking).
#' @param MaxIter default `80`. Number of jumps towards the solution (for logit raking)
#' @param FunTol default `1e-5`. Maximum allowed difference between the raking target and raked results (for logit raking)
#' @param iterate default `F`. If logit raking for a location-year fails, try again with `MaxJump` and `MaxIter` times 10. If that fails, try again times 100. For circumstances where raking target is very far from estimate and raking does not converge.
#' @param modeling_shapefile_version string identifying version of of shapefile used in modeling
#' @param raking_shapefile_version string identifying version of of shapefile to use in raking
#' @param if_no_gbd default `return_na`, other option `return_unraked`. If return_na, any location-years without gbd estimates will return NA raking factors. if return_unraked, will any location-years without gbd estimates will return 1 for linear raking, 0 for logit raking.
#' @param custom_raking_shapefile SPDF object -shapefile that will be used for raking. Used for passing in custom raking shapefile for choosing subnational countries to rake to. See `make_custom_raking_shapefile()`
#' @param countries_not_to_subnat_rake as it sounds. Used for constructing raking raster. Default: NULL
#'
#' @return Returns a named list with a raked cell pred object, simple raster used for raking, raking factors, and (optional) rasters of mean, lower, upper, and cirange for years in year list
#'
#' @export
rake_cell_pred <- function(cell_pred,
                           rake_to,
                           reg,
                           year_list,
                           pop_measure,
                           rake_method = "linear",
                           rake_subnational = T,
                           crosswalk = F,
                           shapefile_path = get_admin_shapefile(admin_level = 0, raking = T, version = modeling_shapefile_version),
                           field = "loc_id",
                           zero_heuristic = F,
                           approx_0_1 = F,
                           simple_raster = NULL,
                           simple_polygon = NULL,
                           pop_raster = NULL,
                           modeling_shapefile_version = "current",
                           raking_shapefile_version = "current",
                           if_no_gbd = "return_na",
                           custom_raking_shapefile = NULL,
                           countries_not_to_subnat_rake = NULL,
                           ...) {
  if (!rake_method %in% c("linear", "logit")) {
    stop("rake_method must be either linear or logit")
  }

  if (!is.null(simple_raster) & is.null(simple_polygon) & is.null(pop_raster)) {
    stop("If you want to pass in a simple raster directly, you must also pass in either a simple_polygon or pop raster")
  }

  if (length(year_list) == 1) {
    interval_mo <- 12
  } else {
    # Calculate interval month, number of months between years in year list
    year_diff <- diff(year_list)
    if (length(unique(year_diff)) != 1) {
      stop("Please use annual or 5-year intervals exclusively in year_list")
    } else {
      interval_mo <- year_diff[[1]] * 12
    }
  }

  message("\n        Starting Raking for Region: ", reg)
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  message("     Loading Necessary Shapefiles and Rasters")
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  ## load objects used in modeling
  if (is.null(simple_raster)) {
    ## get simple polygon and simple raster used to produce cell pred
    message("Loading simple polygon")
    simple_polygon <- load_simple_polygon(
      gaul_list = get_adm0_codes(reg,
        shapefile_version = modeling_shapefile_version
      ),
      buffer = 0.4,
      shapefile_version = modeling_shapefile_version
    )
    subset_shape <- simple_polygon[["subset_shape"]]
    simple_polygon <- simple_polygon[["spoly_spdf"]]

    message("Loading simple raster\n")
    raster_list <- build_simple_raster_pop(subset_shape)
    simple_raster <- raster_list[["simple_raster"]]
  } else {
    message("Using supplied simple raster")
  }

  ## now we load objects to be used in raking
  gaul_list <- get_adm0_codes(reg, shapefile_version = raking_shapefile_version)

  # if not raking subnationally, use the admin0 shapefile
  if (rake_subnational == F) {
    message("You have chosen not to rake subnationally; make sure you are using national level raking factors from GBD\n")
    message("Loading national raking shapefile")
    shapefile_path <- get_admin_shapefile(admin_level = 0, raking = F, version = raking_shapefile_version)

    location_metadata <- get_location_code_mapping(shapefile_version = raking_shapefile_version)
    location_metadata <- location_metadata[, c("GAUL_CODE", "loc_id")]

    shapefile <- readOGR(shapefile_path, stringsAsFactors = FALSE, GDAL1_integer64_policy = TRUE)
    shapefile <- shapefile[shapefile$ADM0_CODE %in% gaul_list, ]

    # merge on loc_ids for making simple raster
    shapefile <- sp::merge(shapefile, location_metadata, by.x = "ADM0_CODE", by.y = "GAUL_CODE", all.x = T)
    shapefile@data[is.na(shapefile@data$loc_id), "loc_id"] <- -1
  } else {
    # loading in subnational shapefile and subsetting for speed
    if (is.null(custom_raking_shapefile)) {
      message("Loading subnational raking shapefile")
      shapefile <- readOGR(shapefile_path, stringsAsFactors = FALSE, GDAL1_integer64_policy = TRUE)
    } else {
      message("Using custom raking shapefile")
      shapefile <- custom_raking_shapefile
    }
    gaul_list <- get_adm0_codes(reg, shapefile_version = raking_shapefile_version)
    shapefile <- shapefile[shapefile$ADM0_CODE %in% gaul_list, ]
    shapefile@data[, field] <- as.numeric(as.character(shapefile@data[, field]))
  }

  # get simple raster from new gbd shapefile
  message("Loading raking raster\n")
  new_simple_polygon <- load_simple_polygon(
    gaul_list = NULL, ## doesn't matter since custom_shapefile specified
    buffer = 0.4, custom_shapefile = shapefile
  )
  new_subset_shape <- new_simple_polygon[["subset_shape"]]
  new_simple_polygon <- new_simple_polygon[["spoly_spdf"]]

  message("Loading simple raster\n")
  raking_link_table <- build_raking_link_table(shapefile_version = raking_shapefile_version, force_adm0 = !rake_subnational, countries_not_to_subnat_rake = countries_not_to_subnat_rake)
  new_raster_list <- build_simple_raster_pop(new_subset_shape, field = field, link_table = raking_link_table)
  new_simple_raster <- new_raster_list[["simple_raster"]]

  # get extents of original and simple raster to line up - extend and crop just in case
  new_simple_raster <- extend(new_simple_raster, simple_raster, values = NA)
  new_simple_raster <- crop(new_simple_raster, extent(simple_raster))
  new_simple_raster <- mask(new_simple_raster, simple_raster)

  # pulling values from raster into a list for comparison
  simple_extract <- raster::extract(simple_raster, extent(simple_raster))
  new_simple_extract <- raster::extract(new_simple_raster, extent(new_simple_raster))
  simple_extract <- na.omit(simple_extract)
  new_simple_extract <- na.omit(new_simple_extract)

  # test that simple raster matches cell pred
  if ((length(simple_extract) * length(year_list)) != nrow(cell_pred)) {
    stop(paste0("the simple raster for region: ", reg, " does not match the cell pred object - make sure that the code for the simple raster has not changed since running the function."))
  }

  # force crosswalk if unfortunate misshape happens:
  if (((length(new_simple_extract) * length(year_list)) != nrow(cell_pred)) | (length(cellIdx(simple_raster)) != length(cellIdx(new_simple_raster)))) {
    warning("cellIdx not aligned. Forcing crosswalk to TRUE because this will fail moving onwards. Please use fractional raking :(")
    crosswalk <- TRUE
  }

  # see documentaion for details on crosswalk
  if (crosswalk) {
    message("crosswalk = T; now altering cell pred to match raking raster")
    message("Warning: This can drop or introduce NA rows into cell pred, see")
    message("crosswalk_cell_pred_add_NA() for more information \n")
    cell_pred <- crosswalk_cell_pred_add_NA(simple_raster, new_simple_raster, cell_pred, year_list)

    # make sure crosswalk worked
    if ((length(new_simple_extract) * length(year_list)) != nrow(cell_pred)) {
      stop(paste0("the simple raster for region: ", reg, " does not match the cell pred object - Problem with Crosswalk"))
    }
  } else {
    # make sure the number of non-na pixels in simple raster and raking raster are the same
    if (length(simple_extract) != length(new_simple_extract)) {
      stop(paste0("simple raster for ", reg, " has a different number of non-na pixels than the raking raster - use crosswalk = T in subnational_rake function call"))
    }
  }

  if (is.null(pop_raster)) {
    message("Loading population raster")
    ## Pull 2000-2015 annual population brick using new covariates function
    if (class(year_list) == "character") year_list <- eval(parse(text = year_list))
    pop_raster_annual <- load_and_crop_covariates_annual(
      covs = "worldpop",
      measures = pop_measure,
      simple_polygon = simple_polygon,
      start_year = min(year_list),
      end_year = max(year_list),
      interval_mo = as.numeric(interval_mo),
      agebin = 1
    )[[1]]
  } else {
    message("Using supplied population raster")
    pop_raster_annual <- pop_raster
  }

  ## extend and crop pop raster to ensure it matches the raking raster
  pop_raster_annual <- raster::extend(pop_raster_annual, new_simple_raster, values = NA)
  pop_raster_annual <- raster::crop(pop_raster_annual, extent(new_simple_raster))
  pop_raster_annual <- raster::setExtent(pop_raster_annual, new_simple_raster)
  pop_raster_annual <- raster::mask(pop_raster_annual, new_simple_raster)

  ## check to ensure the pop raster matches the raking raster in extent and resolution
  if (extent(pop_raster_annual) != extent(new_simple_raster)) {
    stop("population raster extent does not match simple raster")
  }
  if (any(res(pop_raster_annual) != res(new_simple_raster))) {
    stop("population raster resolution does not match simple raster")
  }

  message("\n     Finished loading shapefiles and rasters")
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  message("     Calculating and Applying Raking Factors")
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  # calculate raking factors
  message("rake_method ", rake_method, " was specified\n")
  message("Calculating raking factors\n")
  rf <- calculate_raking_factors(cell_pred,
    rake_to,
    lyv = c("name", "year", "mean"),
    year_list,
    simple_raster = new_simple_raster,
    weight_brick = pop_raster_annual,
    rake_method = rake_method,
    zero_heuristic = zero_heuristic,
    approx_0_1 = approx_0_1,
    if_no_gbd = if_no_gbd,
    ...
  )

  # apply raking factors
  message("Raking cell pred object")
  raked_cell_pred <- apply_raking_factors(cell_pred,
    simple_raster = new_simple_raster,
    rake_dt = rf,
    rake_method = rake_method,
    force_simp_ras_dt_match = F
  )


  message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  message("     Raking for Region: ", reg, " Complete")
  message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

  outputlist <- list(
    "raked_cell_pred" = raked_cell_pred,
    "new_simple_raster" = new_simple_raster,
    "simple_raster" = simple_raster,
    "raking_factors" = rf
  )

  return(outputlist)
}
