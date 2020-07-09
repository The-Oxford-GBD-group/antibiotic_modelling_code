#' @title Shapefile to SpatialPointsDataFrame
#'
#' @description Converts a shapefile into a SpatialPointsDataFrame for a given field
#'
#' @param sp.df SpatialPolygonDataFrame. Input admin shapefile
#' @param field String. The field describing the shapes, for e.g. ADM0_CODE
#'
#' @return A SpatialPointsDataFrame with coords and value of field
#'
#' @importFrom sp SpatialPointsDataFrame
#' @export
#'
shape_to_spointsdf <- function(sp.df, field) {

  ## Loop over 'field' units
  each_country <- lapply(c(1:length(sp.df@polygons)), function(i) {
    ## Loop over polygons for field 'i'
    rbindlist(lapply(c(1:length(sp.df@polygons[[i]]@Polygons)), function(j) {
      data.table(sp.df@polygons[[i]]@Polygons[[j]]@coords, V3 = sp.df@data[[field]][i])
    }))
  })


  results <- data.frame(rbindlist(each_country))
  results <- sp::SpatialPointsDataFrame(coords = results[, 1:2], data = data.frame(field = results[, 3]))


  ## Fix field name to the string supplied
  names(results) <- paste0(field)

  return(results)
}
