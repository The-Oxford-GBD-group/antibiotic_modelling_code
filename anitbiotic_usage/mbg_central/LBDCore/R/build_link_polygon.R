#' @title Build Link Polygon
#'
#' @description Builds a polygon where there is a layer for each pixel in a raster,
#' with an ID field referring to the id of the pixel in the raster
#'
#' @param region character string of region to be pulled from \link{get_adm0_codes()}
#' @param simple_raster default \code{NULL}. provides option to pass in simple raster rather
#' than making it using a region name to save time.
#' @param shapefile_version string to identify version of shapefile to build simple_raster
#'
#' @return returns an sf polygon
#'
#' @examples
#' \dontrun{
#' idpoly <- build_link_polygon(region)
#' }
#' 
#' @importFrom spex polygonize
#' @export
build_link_polygon <- function(region, simple_raster = NULL, shapefile_version = "current") {

  # load simple raster (pass in as parameter for speed up)
  if (is.null(simple_raster)) {
    simple_polygon <- load_simple_polygon(
      gaul_list = get_adm0_codes(region, shapefile_version = shapefile_version),
      buffer = 0.4,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon[["subset_shape"]]
    simple_polygon <- simple_polygon[["spoly_spdf"]]

    message("Loading simple raster")
    raster_list <- build_simple_raster_pop(subset_shape) # ,u5m=TRUE)
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]
  }

  # make a copy of simple raster and set all values to 0
  new_ras <- simple_raster
  values(new_ras) <- 0
  # give each pixel a unique id
  new_ras[1:length(simple_raster)] <- 1:length(simple_raster)

  # convert raster to sf polygon and give it an ID field
  message("converting ID raster to polygon")
  idpoly <- spex::polygonize(new_ras)
  idpoly$ID <- 1:nrow(idpoly)

  return(idpoly)
}
