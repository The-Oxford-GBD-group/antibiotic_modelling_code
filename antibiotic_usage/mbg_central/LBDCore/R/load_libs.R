#' @title Load libraries
#'
#' @description A function to load libraries based on whether the
#' code is being run on cluster prod or the geos nodes. Tries
#' to load each package, and returns errors or warnings if the package fails to
#' load rather than breaking the loop.
#' @note TO BE DEPRECATED BECAUSE LBDCore HAS PACKAGE LIST in dependencies
#' @author Rebecca Stubbs
#'
#' @param packages A vector or list of packages
#' @param stop Boolean; if T will stop code if not all packages load. if F (default),
#' the code will contiue and will throw a warning.
#' @return None
#' @export
load_libs <- function(packages, stop = F) {
  set_root() # Seting the "root" based on OS
  package_lib <- ifelse(grepl("geos", Sys.info()[4]),
    paste0(root, "temp/geospatial/geos_packages"),
    paste0(root, "temp/geospatial/packages")
  )
  .libPaths(package_lib)
  message(paste0("Loading packages from ", package_lib))

  if (Sys.info()[1] == "Windows") {
    stop("STOP! you will overwrite these packages if you run from windows")
  }

  # Try loading each of the packages in the list provided
  for (package in c(packages)) {
    try({
      library(package, lib.loc = package_lib, character.only = TRUE)
    }, silent = T)
  }

  failed_packages <- packages[!packages %in% (.packages())]

  if (length(failed_packages > 0)) {
    message(paste0(
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n",
      "The following packages failed to load: \n",
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"
    ))
    message(paste0(" ", failed_packages))
    message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n")
    if (stop) {
      stop("Code halted; important packages failed to load!")
    } else {
      warning("Some packages did not load properly (see summary above), but we are going to proceed anyway.")
    }
  } else {
    message(paste0(
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n",
      "Packages have been loaded! \n",
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n"
    ))
  }
}
