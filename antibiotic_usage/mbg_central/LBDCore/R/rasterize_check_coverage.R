#' @title Rasterize with border checks
#'
#' @description Rasterizing using a shapefile and a template raster, such that
#' we account for any pixels that are on the border of \code{field} units, which
#' could have been lost due to raster::rasterize only evaluating on centroids
#'
#' @param shapes SpatialPolygonDataFrame.. Input shapefile
#'
#' @param template_raster SpatialPolygonDataFrame.. The reference raster (usually WorldPop)
#'
#' @param field String The field with appropriate administrative unit information (usually ADM0_CODE)
#'
#' @param link_table String or data.table. If data.table it is used as-is. If String: either an absolute
#'   file path to an RDS file OR a short name for the administrative shape file e.g., "2019_02_27" or "current".
#'
#' @return A raster with border and insides properly filled
#'
#' @details rasterize_check_coverage has three distinct use cases based off of the value of link_table
#'
#' 1. \code{link_table} is NULL. In this case rasterize_check_coverage will behave identically to raster::rasterize
#'
#' 2. \code{link_table} is a String referring to relase of admin shapefiles ("current" or e.g., "2019_02_27"). In this case
#'    \code{field} should be "ADM0_CODE", "ADM1_CODE" or "ADM2_CODE". This will load the lbd_standard_link.rds file,
#'    from the related admin shapefile directory, aggregate area_fraction as necessary to match the level of \code{field},
#'    and then apply those values to pixels in the space defined by \code{shapes}.
#'
#' 3. \link{link_table} is a data.table OR a String absolute path to a RDS file containing a data.table. This will use the
#'    provided \code{link_table} to assign values to the result raster similarly to use case #2.
#'
#' Note that for both use cases 2 and 3 all pixel_id coordinates must be in the same raster space. This is currently the
#' area defined by cropping the world raster to the pixels occupied by stage 1 and stage 2 countries.
#'
#' @export
#'
rasterize_check_coverage <- function(shapes, template_raster, field, ..., link_table = modeling_shapefile_version) {
  # backwards-compatible behavior - just call rasterize()
  if (is.null(link_table)) return(raster::rasterize(shapes, template_raster, field = field, ...))

  # Validate arguments
  is_admin_link_table <- FALSE
  if (is.data.table(link_table)) {
    is_admin_link_table <- TRUE
    # nothing to do - already a link table loaded in memory
  } else if (R.utils::isAbsolutePath(link_table)) {
    link_table <- readRDS(link_table)
  } else if (is_admin_shapefile_string(link_table)) {
    is_admin_link_table <- TRUE
    # load link table with pre-computed ownership percentages for each pixel cell
    link_table_file <- paste0(get_admin_shape_dir(link_table), "lbd_standard_link.rds")
    link_table <- readRDS(link_table_file)
  } else {
    stop("link_table argument was neither a data.table, an admin shapefile string, or an absolute path to a RDS file.")
  }

  if (!field %in% names(link_table)) {
    msg <- paste(
      "WARNING: rasterize_check_coverage called with field", field,
      "which is not present in link_table. Defaulting to raster::rasterize()"
    )
    message(msg)
    return(raster::rasterize(shapes, template_raster, field = field, ...))
  }

  # aggregate link table generically for admin 0/1/2
  # Note: we need `with=FALSE` because `field` is passed as a parameter (not a hard-coded string)
  table <- link_table[, c("pixel_id", field, "area_fraction"), with = FALSE]
  if (is_admin_link_table && field != "ADM2_CODE") {
    # sum rows; area_fraction now represents the total area coverage by ADM0/1_CODE instead of ADM2_CODE
    table <- table[, .(area_fraction = sum(area_fraction)), by = c("pixel_id", field)]
  }
  # subset table so that we have 1 entry per pixel_id - the value of `field` with the maximum
  # area_fraction value for that pixel_id
  # https://stackoverflow.com/a/24558696
  pixel_owner <- table[table[, .I[which.max(area_fraction)], by = pixel_id]$V1]
  pixel_owner <- pixel_owner[order(pixel_id)]

  # generate world raster with pixel values for `field`
  world_pixel_owner <- suppressWarnings(empty_world_raster())
  # subset to only those pixels owned by a shape we're interested in
  owned_pixels <- pixel_owner[pixel_owner[[field]] %in% shapes[[field]]]
  world_pixel_owner[owned_pixels$pixel_id] <- owned_pixels[[field]]

  result <- raster::crop(world_pixel_owner, template_raster, snap = "near")
  if (raster::ncell(result) != raster::ncell(template_raster)) {
    message <- paste(
      "Error in creating result raster. Should have created a raster of shape",
      paste(dim(result), collapse = ","),
      "but instead created a raster of shape",
      paste(dim(template_raster), collapse = ",")
    )
    stop(message)
  }
  return(result)
}
