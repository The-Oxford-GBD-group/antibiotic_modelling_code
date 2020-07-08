#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param packages PARAM_DESCRIPTION
#' @param versions PARAM_DESCRIPTION, Default: NULL
#' @param lib PARAM_DESCRIPTION, Default: .libPaths()[1]
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[versions]{installed.versions}}
#'  \code{\link[withr]{with_libpaths}}
#'  \code{\link[devtools]{reexports}}
#' @rdname tidyInstall
#' @export
#' @importFrom versions installed.versions
#' @importFrom withr with_libpaths
#' @importFrom devtools install_version
tidyInstall <- function(packages, versions = NULL, lib = .libPaths()[1]) {
  # given a vector of package names and optional vector of version numbers
  # install them from CRAN if they aren't already loaded

  npkg <- length(packages)

  # check the arguments
  stopifnot(inherits(packages, "character"))

  if (!is.null(versions)) {
    stopifnot(length(versions) == npkg)
    stopifnot(inherits(versions, "character"))
  }

  # run a loop if multiple required
  if (npkg > 1) {
    for (i in 1:npkg) {
      if (is.null(versions)) {
        versions_tmp <- NULL
      } else {
        versions_tmp <- versions[i]
      }

      tidyInstall(packages[i], versions_tmp, lib)
    }
  } else {
    # otherwise just for one

    # first make sure versions and devtools are installed
    for (pkg in c("versions", "devtools")) {
      if (!(pkg %in% installed.packages(lib.loc = lib))) {
        install.packages(pkg, lib = lib)
      }
    }

    # check whether the package is installed
    installed <- packages %in% rownames(installed.packages(lib.loc = lib))

    if (installed && is.null(versions)) {

      # if it is installed the user doesn't care about the version exit
      return(invisible())
    } else {

      # otherwise, do some installing...

      # if it's installed, check the version
      if (installed) {
        installed_version <- versions::installed.versions(packages, lib = lib)

        # if that's the required version, exit
        if (versions == installed_version) {
          return(invisible())
        } else {
          # otherwise, install the correct version
          withr::with_libpaths(
            new = lib,
            devtools::install_version(packages, versions)
          )
        }
      } else {
        # if it isn't installed ...

        if (!is.null(versions)) {
          # if a version is required, install it and exit
          withr::with_libpaths(
            new = lib,
            devtools::install_version(packages, versions)
          )
          return(invisible())
        } else {
          # otherwise get the most recent version and exit
          install.packages(packages, lib = lib)
          return(invisible())
        }
      }
    }
  }
}
