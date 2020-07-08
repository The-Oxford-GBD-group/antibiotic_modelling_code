
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param buffer PARAM_DESCRIPTION
#' @param tolerance PARAM_DESCRIPTION, Default: 0.2
#' @param subset_only PARAM_DESCRIPTION, Default: F
#' @param makeplots PARAM_DESCRIPTION, Default: F
#' @param use_premade PARAM_DESCRIPTION, Default: F
#' @param custom_shapefile_path PARAM_DESCRIPTION, Default: NULL
#' @param custom_shapefile PARAM_DESCRIPTION, Default: NULL
#' @param raking PARAM_DESCRIPTION, Default: F
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
#'  \code{\link[rgeos]{gIntersection}}
#'  \code{\link[raster]{area}}
#' @rdname load_simple_polygon
#' @export
#' @importFrom rgeos gIntersection
#' @importFrom raster area
load_simple_polygon <- function(gaul_list, buffer, tolerance = 0.2,
                                subset_only = F, makeplots = F, use_premade = F,
                                custom_shapefile_path = NULL, custom_shapefile = NULL,
                                raking = F, shapefile_version = "current") {
  ## Read in shapefile and dissolve to single polygon for creating one big mesh (no admin1 boundaries)
  #   gaul_list = any
  #   buffer = distance to buffer around object
  #   tolerance = tol in the gSimplify function. Higher # = coarser polygon
  #   use_premade = shouold premade versions of regions / Africa be used?
  #   raking = pull raking shapefile
  #   shapefile_version = string specifying which shapefile version to pull

  # Logic check
  if (!is.null(custom_shapefile_path) & !is.null(custom_shapefile)) stop("You cannot specify both a custom shapefile and a custom shapefile path")

  # If using custom shapefiles, don't use premade polys
  if (!is.null(custom_shapefile_path) | !is.null(custom_shapefile)) use_premade <- F

  if (use_premade == T) {
    # Check for common gaul lists so we can query premade simple polygons
    if (identical(gaul_list, c(29, 42, 45, 47, 50, 66, 90, 94, 106, 105, 144, 155, 159, 181, 182, 214, 217, 221, 243))) {
      message("Your GAUL list matches WSSA, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/wssa.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
    if (identical(gaul_list, c(8, 49, 59, 68, 76, 89))) {
      message("Your GAUL list matches CSSA, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/cssa.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
    if (identical(gaul_list, c(43, 58, 70, 77, 79, 133, 150, 152, 170, 205, 226, 74, 257, 253, 270))) {
      message("Your GAUL list matches ESSA, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/essa.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
    if (identical(gaul_list, c(4, 40762, 40765, 145, 169, 6, 248))) {
      message("Your GAUL list matches NAME, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/name.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
    if (identical(gaul_list, c(35, 142, 172, 227, 235, 271))) {
      message("Your GAUL list matches SSSA, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/sssa.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
    if (identical(gaul_list, c(
      4, 6, 8, 29, 35, 42, 43, 45, 47, 49, 50, 58, 59, 66, 68, 70,
      74, 76, 77, 79, 89, 90, 94, 95, 105, 106, 142, 144, 145, 150,
      152, 155, 159, 169, 170, 172, 181, 182, 205, 214, 217, 221,
      226, 235, 243, 248, 253, 268, 270, 271, 40762, 40765,
      227, 257, 133, 269
    ))) {
      message("Your GAUL list matches AFRICA, loading premade simple_polygon and assigning subset_shape to global environment...")
      load("/share/geospatial/simple_polygons/africa.RData")
      assign("subset_shape", subset_shape, envir = globalenv())
      if (makeplots) plot(spoly_spdf)
      if (makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
      return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
    }
  }

  # Otherwise, make a new simple_poly
  # count the vertices of a SpatialPolygons object, with one feature
  vertices <- function(x) sum(sapply(x@polygons[[1]]@Polygons, function(y) nrow(y@coords)))

  # ~~~~~~~~~~~~~~~~~
  # load data

  if (is.null(custom_shapefile_path) & is.null(custom_shapefile)) {
    message("Opening master shapefile...")
    master_shape <- readOGR(get_admin_shapefile(
      admin_level = 0, raking = raking,
      version = shapefile_version
    ))
    master_shape@data$ADM0_CODE <- as.numeric(as.character(master_shape@data$ADM0_CODE))
    subset_shape <- master_shape[master_shape@data$ADM0_CODE %in% gaul_list, ]
  } else if (!is.null(custom_shapefile_path) & is.null(custom_shapefile)) {
    message("Opening custom shapefile...")
    master_shape <- readOGR(custom_shapefile_path)
    subset_shape <- master_shape
  } else if (is.null(custom_shapefile_path) & !is.null(custom_shapefile)) {
    master_shape <- custom_shapefile
    subset_shape <- master_shape
  }

  if (subset_only == TRUE) {
    return(list(subset_shape = subset_shape, spoly_spdf = NULL))
  }

  if (subset_only == FALSE) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    message("Making a super-low-complexity map outline for INLA mesh creation")

    message(paste0("Full polygon vertices: ", vertices(subset_shape)))

    # Merge everything together (where intersecting)
    af <- gUnaryUnion(subset_shape)

    # Initial simplification
    af_simple <- gSimplify(af, tol = tolerance, topologyPreserve = TRUE)

    # Remove tiny features
    # Get all sub polygons and their areas
    polys <- af_simple@polygons[[1]]@Polygons
    areas <- sapply(polys, function(x) x@area)

    # If there is more than one sub polygon, remove the ditzels (many single-country subsets are a single polygon,
    #   like Uganda, which would break these few lines)
    if (length(areas) > 1) {
      # find top 5% by area
      big_idx <- which(areas > quantile(areas, 0.95))

      # convert back into a spatialPolygons object
      spoly <- SpatialPolygons(list(Polygons(polys[big_idx], ID = 1)))
    }
    if (length(areas) == 1) {
      spoly <- af_simple
      big_idx <- 1
    }

    # Buffer slightly
    spoly <- gBuffer(spoly, width = buffer)

    # simplify again to reduce vertex count
    spoly <- gSimplify(spoly, tol = tolerance, topologyPreserve = TRUE)

    # Get list of original polygons
    polys2 <- af@polygons[[1]]@Polygons

    # Check if all are within the simple polygon
    check_if_in_spoly <- function(the_poly, compare_to, the_proj = projection(master_shape)) {
      the_poly <- SpatialPolygons(list(Polygons(list(the_poly), ID = 1)))
      projection(the_poly) <- the_proj
      projection(compare_to) <- the_proj

      if (suppressWarnings(gIsValid(the_poly)) == F) return(TRUE) # Ignore invalid polygons

      poly_intersect <- rgeos::gIntersection(the_poly, compare_to)

      if (is.null(poly_intersect)) {
        return(FALSE)
      } else {
        return(ifelse((raster::area(poly_intersect) == raster::area(the_poly)), TRUE, FALSE))
      }
    }

    over_list <- sapply(polys2, function(x) check_if_in_spoly(x, compare_to = spoly))

    if (all(over_list) == FALSE) {
      # Add back in polygons if missed by above procedure (e.g. islands dropped)
      big_idx <- unique(c(big_idx, which(over_list == F)))
      spoly <- SpatialPolygons(list(Polygons(polys[big_idx], ID = 1)))
      spoly <- gBuffer(spoly, width = buffer)
      spoly <- gSimplify(spoly, tol = tolerance, topologyPreserve = TRUE)
    }

    # Now check again with new spoly
    over_list <- sapply(polys2, function(x) check_if_in_spoly(x, compare_to = spoly))

    # If still not all enclosed, tolerance probably too high. Return warning
    if (all(over_list) == FALSE) {
      number_false <- length(over_list[over_list == F])
      number_total <- length(over_list)
      warning(paste0(
        number_false, " of ", number_total, " polygons are NOT enclosed within your simple polygon. \n",
        "Adjust your buffer and tolerance values."
      ))
    }

    # Return results
    message(paste0("Simplified vertices: ", vertices(spoly)))

    # plot to check it encloses all of the important bits
    if (makeplots) plot(spoly)
    if (makeplots) plot(af, add = TRUE, border = grey(0.5))

    # turn into an SPDF
    spoly_spdf <- SpatialPolygonsDataFrame(spoly,
      data = data.frame(ID = 1),
      match.ID = FALSE
    )

    # add projection information
    projection(spoly_spdf) <- projection(master_shape)

    return(list(subset_shape = subset_shape, spoly_spdf = spoly_spdf))
  }
}
