#' @title Build Link Table
#'
#' @description: Builds a link table where each row represents an admin2-pixel, and includes
#' the admin0-2 levels, the id of the pixel (based on simple raster), and the percent of the
#' pixel covered by a country. Used for fractional raking where a pixel is in multiple
#' administrative units.
#'
#' @param shapefile_version version of admin file to base link table on
#' @param cores number of cores to run mclapply over
#' @param region the region used to generate the simple raster
#' @param custom_shapefile_path A non-admin shapefile to build a link table for
#' @param custom_shapefile_field the field in custom_shapefile with admin identifiers
#'
#' @return returns the link data.table described above, and the id raster and polygon used in the calculation
#'
#' @examples
#' \dontrun{
#' link <- build_link_table(shapefile_version = "2018_08_01", cores = 10, region = "stage1+stage2")
#' 
#' link <- build_link_table(shapefile_version = NULL, cores = 10, region, region = NULL, custom_shapefile_path = "C:/shapefile.shp", custom_shapefile_field = "ADM_CODE")
#' }
#' 
#' @export
build_link_table <- function(shapefile_version,
                             cores,
                             region = "stage1+stage2",
                             custom_shapefile_path = NULL,
                             custom_shapefile_field = NULL) {

  # load in packages not included in singularity image
  new_pkg_lib <- "/share/code/geospatial/jdv6/r_packages"
  .libPaths(c(.libPaths(), paste0("/share/geospatial/non_lbd_rstudio_pkgs/", paste0(R.Version()[c("major", "minor")], collapse = ".")), new_pkg_lib))
  test_pkg_list <- c("sf", "lwgeom", "RhpcBLASctl")
  for (pkg in test_pkg_list) {
    if (!pkg %in% as.vector(installed.packages(lib.loc = .libPaths())[, "Package"])) {
      install.packages(pkg, lib = new_pkg_lib)
    }
  }
  lapply(test_pkg_list, library,
    character.only = TRUE
  )

  if (is.null(custom_shapefile_path)) {
    # read the admin 2 shapefile
    polys <- sf::st_read(get_admin_shapefile(admin_level = 2, version = shapefile_version), stringsAsFactors = FALSE)

    # subset admin 2 shapefile to desired countries
    s1 <- get_adm0_codes(region, shapefile_version = shapefile_version)
    polys <- polys[polys$ADM0_CODE %in% c(s1), ]
    simple_polygon_list <- load_simple_polygon(gaul_list = get_adm0_codes(region, shapefile_version = shapefile_version), buffer = 0.4, subset_only = FALSE)
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    message("Building simple raster from subset_shape")
    raster_list <- build_simple_raster_pop(subset_shape)
  } else {
    region <- NULL
    polys <- st_read(custom_shapefile_path, stringsAsFactors = F, quiet = T)
    simple_polygon_list <- load_simple_polygon(gaul_list = NULL, buffer = 0.4, subset_only = FALSE, custom_shapefile_path = custom_shapefile_path)
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    message("Building simple raster from subset_shape")
    raster_list <- build_simple_raster_pop(subset_shape, field = custom_shapefile_field, link_table = NULL)
  }

  # landsea mask
  landsea <- raster("/home/j/WORK/11_geospatial/01_covariates/02_Oxford/01_Global_Masks/Land_Sea_Masks/CoastGlobal_5k_float.tif")
  # create a raster version of the pixel_id

  simple_raster <- raster_list[["simple_raster"]]

  # create pixel_id raster
  px_ras <- simple_raster
  px_ras[] <- 1:length(px_ras)

  # build link polygon, where there is a polygon for each pixel identified by pixel_id
  pixels <- build_link_polygon(region, simple_raster = simple_raster)

  # fix some names
  setnames(pixels, "layer", "pixel_id")

  # assign crs
  st_crs(pixels) <- 4326

  get_intersect <- function(poly, x = -1) {
    print(x)
    start <- Sys.time()
    # make sure the polygon has valid geometry. If not, get the computer to fix it magically
    if (!st_is_valid(poly)) {
      poly <- st_make_valid(poly)
    }

    # crop the baseline raster
    poly_bb <- st_bbox(poly)
    poly_extent <- extent(poly_bb[1], poly_bb[3], poly_bb[2], poly_bb[4])
    px_crop <- crop(px_ras, poly_extent, snap = "out")
    px_crop <- px_crop[]

    # subset possible pixels
    target <- pixels[pixels$pixel_id %in% px_crop, ]

    # get a list of target polys (e.g. zip code) that intersect with the poly (e.g. critical habitat)
    inters <- st_intersects(poly, target)

    # calculate start area of the poly
    target <- target[inters[[1]], ]
    target$start_area <- st_area(target)

    # calculate the intersection
    aaa <- st_intersection(poly, target)

    # if there is an intersection, calculate the end area, remove the geometry to save space

    if (nrow(aaa) > 0) {
      aaa$end_area <- st_area(aaa)

      st_geometry(aaa) <- NULL

      setDT(aaa)
      ret_obj <- aaa
      ret_obj[, area_fraction := end_area / start_area]
    } else {
      st_geometry(poly) <- NULL
      ret_obj <- poly
      setDT(ret_obj)
    }
    end <- Sys.time()

    ret_obj$start <- start
    ret_obj$end <- end

    return(ret_obj)
  }

  RhpcBLASctl::blas_set_num_threads(1)

  # go through id polygons in parallel and calculate intersections with ADM2 polygon
  link_fxed_gaul <- parallel::mclapply(1:nrow(polys),
    function(x) get_intersect(polys[x, ], x = x),
    mc.preschedule = F, mc.cores = cores
  )

  link_fxed_gaul <- rbindlist(link_fxed_gaul)
  # backwards compatibility
  link_fxed_gaul[, ID := pixel_id]
  # converting from "unit" to numeric
  link_fxed_gaul$area_fraction <- as.numeric(link_fxed_gaul$area_fraction)
  # fixes area_fractions for pixels that are partially in water
  link_fxed_gaul <- fix_link(link_fxed_gaul)

  return(list("link_table" = link_fxed_gaul, "id_raster" = px_ras, "id_poly" = pixels))
}
