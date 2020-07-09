#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getpolys
#' @export
getpolys <- function(i) {
  # pull shapefile
  shape <- shapefiles[i]
  message(paste0("Working on shapefile: ", shape))
  shp <- shapefile(paste0(shp_path, "/", shape, ".shp"))

  # HOTFIX for zambia shapefile with funny gaul codes
  if (shape == "GRED_Zambia") {
    shp@data$GAUL_CODE <- gsub("894.2006.", "", shp@data$GAUL_CODE)
  }

  # get location codes as numeric, not factor
  loc_codes <- unique(shapes[shapes$shapefile == shape, ]$location_code) %>%
    as.character() %>%
    as.numeric()

  polys_subset <- list()
  problem_shapes <- c() # ensure errors due to problematic shapes are captured and reported to user

  for (j in 1:length(loc_codes)) {
    code <- loc_codes[j]
    poly_name <- paste0(shape, "__", code)

    if (code %in% as.character(shp$GAUL_CODE)) {
      poly <- shp[as.character(shp$GAUL_CODE) == code, ]
      polys_subset[[poly_name]] <- poly
    } else {
      problem_shapes <- c(problem_shapes, poly_name)
      warning(sprintf("GAUL code: %s not found in shapefile: %s", code, shape))
    }
  }

  if (length(problem_shapes) >= 1) {
    polys_subset[["problem_shapes"]] <- problem_shapes
  } else {
    polys_subset[["problem_shapes"]] <- NA
  }
  return(polys_subset)
}
