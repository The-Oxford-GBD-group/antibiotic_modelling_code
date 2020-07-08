#' @title Return logical indicating if string is an admin shapefile version string.
#'
#' @description Checks strings to see if they are equal to "current" or match a YYYY_MM_DD format.
#' If so, returns TRUE. Else FALSE. No checking is done to ensure that the date string
#' has a corresponding subdirectory in the admin_shapefiles directory.
#'
#' @param s String the string to check.
#'
#' @export
is_admin_shapefile_string <- function(s) {
  if (s == "current") {
    TRUE
  } else if (length(grep("^\\d{4}_\\d{2}_\\d{2}$", s))) {
    TRUE # "2019_02_27" or another admin shapefile release date
  } else {
    FALSE
  }
}
