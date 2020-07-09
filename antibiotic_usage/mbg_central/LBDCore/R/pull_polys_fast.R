#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param shapefiles PARAM_DESCRIPTION
#' @param shapes PARAM_DESCRIPTION
#' @param shp_path PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname pull_polys_fast
#' @export
pull_polys_fast <- function(shapefiles, shapes, shp_path) {
  message("Extracting all polygons from shapefiles on disk -- in serial from .rds files.\n")

  # run in serial by shapefile with fast poly loading function
  polys <- lapply(shapefiles, function(shape) {
    message(paste0("Working on shapefile: ", shape))
    shp <- fast_load_shapefile(shape)

    # subsetting will break if NAs in row index (GAUL_CODE)
    shp <- shp[!is.na(shp$GAUL_CODE) & (shp$GAUL_CODE != ""), ]

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
  })
  return(polys)
}
