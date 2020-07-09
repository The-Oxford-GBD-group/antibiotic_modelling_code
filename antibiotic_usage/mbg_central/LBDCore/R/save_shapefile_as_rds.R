#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shapefile_path PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @param in_dir PARAM_DESCRIPTION, Default: '/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/'
#' @param out_dir PARAM_DESCRIPTION, Default: '/share/geospatial/rds_shapefiles/'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname save_shapefile_as_rds
#' @export
#' @importFrom stringr str_match
save_shapefile_as_rds <- function(shapefile_path,
                                  verbose = F,
                                  in_dir = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/",
                                  out_dir = "/share/geospatial/rds_shapefiles/") {
  # Function to convert from .shp to RDS

  # Load one function from stringr
  str_match <- stringr::str_match

  shape <- str_match(shapefile_path, "//(.*).shp$")[2]

  lock_file(shape)

  if (verbose == T) {
    message(paste0("     ", shape))
  }

  in_dir_no_slash <- gsub("/$", "", in_dir)

  the_shape <- try(readOGR(dsn = in_dir_no_slash, layer = shape), silent = T)

  if (is.error(the_shape)) {
    unlock_file(shape)
    return(the_shape)
  } else {
    saveRDS(the_shape, file = paste0(out_dir, shape, ".rds"))
    unlock_file(shape)
    return("success")
  }
}
