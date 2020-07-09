## get_sp_hierarchy ######################################################
#' @title Pull the hierarchy of admin codes
#'
#' @description Takes the admin2 shapefile and pulls a list of
#' admin level codes to be used in merges, etc.
#'
#' @param shapefile_version character string indicating version of shapefile to pull
#'
#' @return a list of 3 data tables (adm0, adm1, adm2). Each data table includes
#'  the location codes of its administrative level and columns for the higher-
#'  order administrative levels as well.
#'
#'
#' @export
get_sp_hierarchy <- function(shapefile_version = "current") {
  admin_level <- 2

  # Use most up to date admin2 files - in rds format for faster loading
  admin_shp <- rgdal::readOGR(get_admin_shapefile(admin_level, version = shapefile_version),
    stringsAsFactors = FALSE, GDAL1_integer64_policy = TRUE
  )

  # Pull admin codes
  admin_data <- copy(data.table(admin_shp@data))
  admin_codes <- lapply(0:2, function(aa) {
    col_list <- sapply(0:aa, function(adm) {
      return(c(
        paste0("ADM", adm, "_CODE"),
        paste0("ADM", adm, "_NAME")
      ))
    })
    col_list <- as.vector(col_list)
    data_subset <- subset(admin_data, select = col_list)
    data_subset <- unique(data_subset)
    return(data_subset)
  })

  names(admin_codes) <- c("ADM0", "ADM1", "ADM2")

  return(admin_codes)
}
