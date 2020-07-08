#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param xy PARAM_DESCRIPTION
#' @param shapes PARAM_DESCRIPTION
#' @param shape_ident PARAM_DESCRIPTION, Default: 'gaul_code'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_sample_counts
#' @export
get_sample_counts <- function(ss = 1,
                              xy,
                              shapes,
                              shape_ident = "gaul_code",
                              ...) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## shapefile countss
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  ## this function takes in data and relevant shapefiles and returns
  ## how many of the datapoints falls into each of the shapefiles as
  ## well as the mapping vector between points and the shape_ident
  ## of the polygons

  ## data: full dataset
  ## ss: vector of sample sizes in each row. if <1> it assumes all have ss=1
  ## xy: matrix of xy coordinates
  ## shapes: all relevant loaded shapefiles (e.g loaded visa raster::shapefiles() )
  ## shape_ident: in a spatial polygons dataframe, there is data associated with each
  ##    entry. this variable identifies which of these data cols you'd like to use to
  ##    refer to the different polygons in the SPDF. if using admin2, leave as "gaul_code"
  ##    but, if you make your own set of shapes, you may want to select another col


  data <- cbind(xy, ss)
  colnames(data) <- c("long", "lat", "ss")

  ## make sure all relevant cols are truly numeric
  for (i in 1:3) {
    data[, i] <- as.numeric(as.character(data[, i]))
  }

  data <- as.data.frame(data)

  ## prepare the data to extract points into the shapefile
  coordinates(data) <- ~ long + lat
  proj4string(data) <- proj4string(shapes)

  ## find which shapes the data falls into
  ## this takes the longest
  row_shapes <- over(data, shapes)
  row_shapes <- row_shapes[, shape_ident]

  ## WARNING! If some of your pts don't fit into the poly shapes
  ## they get randomly assigned to folds at the end
  ## TODO: map these to nearest polys somehow
  if (sum(is.na(row_shapes)) > 0) {
    message("Warning!! Some of your pts don't fall into any of the polygon shapes!!")
    message("They will get randomly assigned to folds")
    png("~/pts_outside_polys.png", width = 1080, height = 1080)
    plot(shapes)
    points(data[which(is.na(row_shapes)), ], col = "red", pch = 3)
    dev.off()
  }

  ## find the sample size in each shape
  all_shapes <- sort(unique(shapes@data[, shape_ident]))
  n_shapes <- length(all_shapes)
  samp_size_shape <- cbind(all_shapes, rep(NA, n_shapes))
  for (i in 1:n_shapes) {
    shape_id <- samp_size_shape[i, 1]
    data_rows <- which(row_shapes == shape_id)
    samp_size_shape[i, 2] <- sum(data$ss[data_rows])
  }
  colnames(samp_size_shape) <- c(shape_ident, "count")

  return(list(
    ct_mat = samp_size_shape,
    pt_poly_map = row_shapes
  ))
}
