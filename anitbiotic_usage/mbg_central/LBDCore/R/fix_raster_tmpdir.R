#' @title Set temporary directory used by the raster package and clear old files.
#'
#' @description \code{fix_raster_tmpdir()} loads and configures the raster package.
#'
#' @author Mike Richards, \email{miker985@uw.edu}
#'
#' @details
#' By default the raster package uses /tmp to store temporary files by default.
#' This is problematic as IHME machines are not configured to have a large
#' amount of /tmp space, and multiple users will quickly fill the directory
#' leading to a non-functioning computer. This function does two things: set the
#' temporary directory to /share/scratch/tmp/geospatial-tempfiles/$USER (a location
#' agreed to by IHME infrastructure) and also delete all files a week or older
#' in that directory owned by whomever is running the function.
#'
#' @return NULL
#'
#' @seealso This is called by:
#' \code{\link{mbg_setup}}
#'
#' @importFrom raster rasterOptions removeTmpFiles
#' @export
fix_raster_tmpdir <- function() {
  # Give the user a message about what is happening
  message("Loading and configuring the raster package...")
  # set temporary file dir. INFR does not regularly delete files from here
  raster_tmp_dir <- paste("/share/scratch/tmp/geospatial-tempfiles", Sys.getenv("USER"), sep = "/")
  if (!dir.exists(raster_tmp_dir)) dir.create(raster_tmp_dir)
  raster::rasterOptions(tmpdir = raster_tmp_dir)
  # delete files older than 25 days (maximum days in geospatial.q)
  if (!interactive()) raster::removeTmpFiles(h = 24 * 25)
}
