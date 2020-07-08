## mbg_setup() ---------------------------------------------------------------->
#' @title Loads R packages and sources functions for MBG modeling
#'
#' @description
#' \code{mbg_setup()} Loads R packages and sources functions for MBG modeling
#' with helper functions in 'MBG setup functions' family
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' This function makes use of some other helper functions in the 'MBG setup
#' functions' family to load all the necessary packages in a provided vector
#' as well as functions for the core_repo and optionally additional repos such
#' as indicator repos and/or development repos. Functions will be read in from
#' repos in the order they appear in the 'repos' vector argument. The examples
#' below show how this could be run with one or multiple repos.
#'
#' @param package_list A vector of R package names to attempt to load
#' @param repos A vector of paths to repository directories. At a
#'   minimum, this should contain the 'core_repo', but may also contain an
#'   indicator repo and one or more additional repos. Functions will be loaded
#'   from each repo in the order they appear in this vector. If any path has
#'   'lbd_core' as the last directory in the path, only it's subdirectory
#'   'mbg_central' will be recursively searched for functions to load as is the
#'   custom. A single, complete path (or object storing a single path) can also
#'   be provided as a string.
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function uses:
#' \code{\link{load_R_packages}}
#' \code{\link{load_mbg_functions}}
#' \code{\link{is_lbd_core_repo}}
#'
#' @examples
#' \dontrun{
#' mbg_setup(package_list = "ggplot2", repos = "/share/code/geospatial/lbd_core")
#' mbg_setup(package_list = package_list, repos = c(core_repo, indic_repo))
#' }
#' @export
mbg_setup <- function(package_list, repos) {
  # check 'package_list' and 'repos' list to make sure they are of correct type
  # and actually contain something
  if (!is.character(package_list) | length(package_list) == 0) {
    stop("Package list not characer vector and/or empty...\nExiting!")
  }
  # Similarly, check repos list and source functions
  if (!is.character(repos) | length(repos) == 0) {
    stop("Vector of repo paths not character vector and/or empty...\nExiting!")
  }

  # load packages
  load_R_packages(package_list)

  # source in functions
  for (repo in repos) {
    # if you are pointing to the lbd_core repo (or a fork) only search the
    # '/mbg_central' subdirectory for functions to source
    if (is_lbd_core_repo(repo)) repo <- paste0(repo, "/mbg_central")
    # load the functions from the repo
    load_mbg_functions(repo)
  }

  # Loading and configuring the raster package
  fix_raster_tmpdir()

  # If we are in an LBD Singularity image, load "setthreads.so" and set to
  # serial if we are running RStudio
  load_setthreads()
}
