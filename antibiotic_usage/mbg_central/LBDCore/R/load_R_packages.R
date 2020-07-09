#' @title A function to load a list of R packages
#'
#' @description
#' \code{load_R_packages} loads a list of R packages depending on where the
#' script is being run.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @note Deprecated since the library will load all dependent packages
#' @details
#' WARNING: The logic for this function is dependent on the SINGULARITY_NAME
#'          environmental being written into the Renviron in the Singularity
#'          image so that it can be detected from within RStudio. This should
#'          match the name of the image and will therefore be an exact match for
#'          the SINGULARITY_NAME environmental variable created by Singularity
#'          for consistency. Unfortunately, this variable is not accessible from
#'          RStudio, which is why we go to the trouble of hard coding it into
#'          the Renviron.
#'
#' WARNING: In addition to the SINGULARITY_NAME this function also assumes that
#'          all packages built into LBD Singularity images live at:
#'          /usr/local/R-<version>/library
#'
#' This function is an attempt to address the tangle that is loading packages
#' from different locations. This was much worse in the past when we were
#' loading packages for different versions of R installed on different cluster
#' nodes running different OS's as well as RStudio and R both under Singularity
#' images. After the team moved to R and RStudio only running within Singularity
#' containers, the process of determining where to load packages from became
#' much simpler.
#'
#' The following Scenario's are currently supported by this function:
#'
#' Scenario | How is R Run?            | Load Packages From Directory
#' ---------|------------------------------------------------------------------
#'     A    | R in LBD image           | /usr/local/R-<version>/library
#'     B    | RStudio in LBD image     | /usr/local/R-<version>/library
#'     C    | RStudio in non-LBD image | /share/geospatial/non_lbd_rstudio_pkgs/<version>
#'
#' Note that R packages meant for use in RStudio not supported by the LBD team
#' were originally stored at '/home/j/temp/geospatial/singularity_packages'
#' but in October of 2018, were moved to
#' '/share/geospatial/non_lbd_rstudio_pkgs' after an update was made to the
#' cluster where /home/j was not necessarily accessible from all nodes.
#' This function tries to identify each of these scenarios and then load
#' packages from the correct place. Scenario A and B are when running R from
#' within a Singularity container spun up from an image that *has been* built
#' by the LBD team. We want to check for this specifically so we know 1) that
#' the required packages are also built into the image and we know where they
#' are located and 2) that special packages (like TMB) are there.
#'
#' Scenario C would be for those users that continue to use RStudio from
#' images that LBD does not construct or support
#' (/share/singularity-images/rstudio) and only has a limited number of packages
#' built in to the image, which is why we rely on the directory of R packages
#' outside of the image.
#'
#' Once this function determines where it is being run, it will attempt to load
#' the list of packages from the appropriate directory also setting
#' '.libPaths()' as necessary.
#'
#' Packages are loaded with calls to 'base::library()' so we get errors if
#' packages cannot be loaded.
#'
#' @param package_list Character vector defining one or more package names.
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#' \code{\link{mbg_setup}}
#'
#' @export
load_R_packages <- function(package_list) {
  # This central scripts are intended to be run on the cluster, and there are
  # have been issues with packages being overwritten by Windows in the past,
  # so bail if we aren't on a Linux machine as a very basic check. Could also
  # have this check as part of 'mbg_setup()', but specifically loading packages
  # under Windows has really been the issue in the past we want to test for here
  # and we may also want to use this function stand alone outside of
  # 'mbg_setup()'
  if (Sys.info()[1] != "Linux") {
    stop("MBG central scripts should be run on the IHME cluster only...\nExiting!")
  }
  # Not an LBD RStudio running within Singularity container
  if (is_rstudio(check_singularity = TRUE) & !is_lbd_singularity()) {
    dir_name <- paste(R.version$major, R.version$minor, sep = ".")
    # e.g., '/home/.../singularity_packages/3.5.1/'
    package_lib <- paste("/share/geospatial/non_lbd_rstudio_pkgs", dir_name, sep = "/")
    # Create directory if it doesn't exist (e.g., because R was upgraded)
    if (!dir.exists(package_lib)) {
      stop(paste0("You're using a version of R for which no LBD packages have been installed. Please ask the lbd_core team to install packages for ", dir_name))
    }
    message("In RStudio within non-LBD Singularity container...")
    # A Singularity container specifically from an LBD image
    # If the SINGULARITY_NAME environmental has been setup properly (as above) it
    # doesn't matter if this is RStudio or not. LBD Singularity images always have
    # their packages installed at the same location.
  } else if (is_lbd_singularity()) {
    # All packages are in the '/usr/local/R-<version>/library' directory in the
    # LBD Singularity image. In the past, other package directories have snuck
    # in so we enforce this here.
    package_lib <- paste(R.home(), "library", sep = "/")
    if (!dir.exists(package_lib)) {
      stop(paste0(
        "Could not find expected LBD Singularity image package directory: '",
        package_lib, "'...\nExiting!"
      ))
    }
    message(paste0("In LBD Singularity container from image: '", Sys.getenv("SINGULARITY_NAME"), "'"))
    # Outside of Singularity container
  } else {
    stop("Only LBD Singularity images or non-LBD Singularity RStudio images currently supported...\nExiting!")
  }

  # Now set \code{.libPaths()} and load the packages from the 'package_lib'
  # directory
  message(paste0("Loading packages from location: '", package_lib, "'"))
  .libPaths(package_lib)
  # Load all of the packages with invisible \code{lapply()} call so it isn't
  # echoed
  invisible(lapply(package_list, library,
    lib.loc = package_lib,
    character.only = TRUE
  ))
}
