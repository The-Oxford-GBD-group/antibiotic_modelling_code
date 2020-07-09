#' @title Detect admin shapefile date and type
#' @description Detects whether the admin shapefile you are using is gaul or gadm
#' and returns the version/date of the shapefile even if the 'current'
#' version was specified
#'
#' @param shpfile_path path to admin shapefile we want to learn about
#'
#' @return two element named list:
#'   1) list element 'shpfile_type' contains string: either 'gadm' or 'gaul'
#'   2) list element 'shpfile_date' contains the actual date of the
#'   shapefile, even if version='current' was used in shpfile_path
#'
#' @examples
#' \dontrun{
#' detect_shapefile_type(get_admin_shapefile(version = "current"))
#' }
#' @export
detect_adm_shapefile_date_type <- function(shpfile_path = get_admin_shapefile(version = modeling_shapefile_version)) {

  ## resolve the symlink to the path if version='current'
  if (grepl(pattern = "current", x = shpfile_path)) {
    resolve.command <- paste("readlink -f", shpfile_path)
    full.path <- system(resolve.command, intern = TRUE)
  } else {
    full.path <- shpfile_path ## this is faster than resolving if it's an option...
  }

  ## grab the date from the full path
  sf.date <- strsplit(full.path, "/")[[1]][6]

  ## determine if shpfile.date pertains to gaul or gadm

  ## assume versions dated on and after Sept. 1, 2018
  ## (transition.date) are GADM, while dates before transition.date
  ## are GAUL unless otherwise coded in as an exception
  transition.date <- "2018_09_01"
  gaul.exceptions <- c() ## dates after transition.date that are actually GAUL
  gadm.exceptions <- c("2018_08_01") ## dates before transition.date are actually GADMw

  ## determine gaul or gadm
  if (sf.date >= transition.date) {
    sf.type <- "gadm"
  } else {
    sf.type <- "gaul"
  }

  if (sf.date %in% gaul.exceptions) sf.type <- "gaul"
  if (sf.date %in% gadm.exceptions) sf.type <- "gadm"

  return(list(
    shpfile_type = sf.type,
    shpfile_date = sf.date
  ))
}
