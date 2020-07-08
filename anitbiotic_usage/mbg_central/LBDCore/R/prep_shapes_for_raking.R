#' @title Create rasters for raking and aggregation
#' @description This snippet of code is taken from \code{rake_cell_pred} so that
#' we can feed this into making summary statistics later on in the fractional raking code
#'
#' @param reg Region
#' @param modeling_shapefile_version Modeling shapefile version
#' @param raking_shapefile_version Raking shapefile version
#' @param field Location field. Default: "loc_id"
#'
#' @return A list with objects: "simple_raster", "simple_polygon", "subset_shape", "new_simple_raster", "new_simple_polygon", "new_subset_shape"
#'
#' @export
prep_shapes_for_raking <- function(reg, modeling_shapefile_version, raking_shapefile_version, field = "loc_id") {
  
  ## Load simple polygon template to model over (in GADM SPACE)
  gaul_list <- get_adm0_codes(reg, shapefile_version = raking_shapefile_version)
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list, buffer = 0.4,
    shapefile_version = raking_shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]
  
  ## Load list of raster inputs (pop and simple)
  raster_list <- build_simple_raster_pop(subset_shape)
  simple_raster <- raster_list[["simple_raster"]]
  pop_raster <- raster_list[["pop_raster"]]
  pixel_id <- seegSDM:::notMissingIdx(simple_raster)
  
  
  ### Craete raked_simple_raster template
  shapefile_path <- get_admin_shapefile(
    admin_level = 0,
    raking = FALSE,
    version = raking_shapefile_version
  )
  
  ## Get GADM codes
  gaul_list <- get_adm0_codes(reg, shapefile_version = raking_shapefile_version)
  
  # If not raking subnationally, use the admin0 shapefile
  message("Loading national raking shapefile")
  
  location_metadata <- get_location_code_mapping(shapefile_version = raking_shapefile_version)
  location_metadata <- location_metadata[, c("GAUL_CODE", "loc_id")]
  
  shapefile <- readOGR(shapefile_path, stringsAsFactors = FALSE, GDAL1_integer64_policy = TRUE)
  shapefile <- shapefile[shapefile$ADM0_CODE %in% gaul_list, ]
  
  # merge on loc_ids for making simple raster
  shapefile <- sp::merge(shapefile, location_metadata, by.x = "ADM0_CODE", by.y = "GAUL_CODE", all.x = T)
  shapefile@data[is.na(shapefile@data$loc_id), "loc_id"] <- -1
  
  
  # get simple raster from new gbd shapefile
  message("Loading raking raster\n")
  new_simple_polygon <- load_simple_polygon(
    gaul_list = NULL, ## doesn't matter since custom_shapefile specified
    buffer = 0.4, custom_shapefile = shapefile
  )
  new_subset_shape <- new_simple_polygon[["subset_shape"]]
  new_simple_polygon <- new_simple_polygon[["spoly_spdf"]]
  
  message("Loading simple raster\n")
  raking_link_table <- build_raking_link_table(shapefile_version = raking_shapefile_version)
  new_raster_list <- build_simple_raster_pop(new_subset_shape, field = field, link_table = raking_link_table)
  new_simple_raster <- new_raster_list[["simple_raster"]]
  
  # get extents of original and simple raster to line up - extend and crop just in case
  new_simple_raster <- extend(new_simple_raster, simple_raster, values = NA)
  new_simple_raster <- crop(new_simple_raster, extent(simple_raster))
  
  
  ## Return a named list of things we want
  return(list(
    "raster_list" = raster_list,
    "simple_raster" = simple_raster,
    "simple_polygon" = simple_polygon,
    "subset_shape" = subset_shape,
    "new_simple_raster" = new_simple_raster,
    "new_simple_polygon" = new_simple_polygon,
    "new_subset_shape" = new_subset_shape,
    "pixel_id" = pixel_id
  ))
}
