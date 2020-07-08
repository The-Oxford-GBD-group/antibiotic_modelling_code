#' @title Return path to admin shapes directory
#'
#' @description
#' Returns path to the official LBD administrative shape file directory. This
#' actually includes non-shape data that is important for mapping, notably
#' the standard link table and standard id raster.
#'
#' @param version Admin shapefile version to pull
#'
#' @export
#'
get_admin_shape_dir <- function(version = "current") {
  paste0("/snfs1/WORK/11_geospatial/admin_shapefiles/", version, "/")
}
