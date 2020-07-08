#' @title Load the standard link table and return it
#'
#' @description Loads the lbd_standard_link.rds file associated with an admin shapefile.
#'
#' @param admin_version the administrative shapefile version. Either "current" or
#' a valid YYYY_MM_DD string.
#'
#' @return the data.table stored in the RDS file.
#'
#' @export
standard_link_table <- function(admin_version) {
  path <- paste0(get_admin_shape_dir(admin_version), "lbd_standard_link.rds")
  return(readRDS(path))
}
