## setmkldynamic() ------------------------------------------------------------>
#' @title Enables MKL to dynamically change the number of OpenMP threads
#'
#' @description
#' \code{setmkldynamic()} Uses a function in the "setthreads.so" shared library
#' built into LBD Singularity images to enable/disable MKL's ability to change
#' the number of OpenMP threads dynamically.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Uses a function in the "setthreads.so" shared library built into LBD
#' Singularity images to enable/disable MKL's ability to change the number of
#' OpenMP threads dynamically. Since many of the packages we use in R rely on
#' OpenMP we normally want to disable MKL dynamic, which is done by default here
#' (0 is a FALSE, i.e. requests disabling dynamic adjustment, see:
#' https://software.intel.com/en-us/mkl-developer-reference-c-mkl-set-dynamic).
#' The shared library should exist in the LBD Singularity image and should have
#' already been loaded by \code{mbg_setup()}. This function checks to make sure
#' that it is loaded and if not, attempts to use \code{load_setthreads()} to do
#' so. A warning is generated if this is run outside of an LBD Singularity image
#' and no MKL dynamic adjustment is done. This function is normally used along
#' with \code{setompnested()} as follows:
#' \code{setmkldynamic(enable = FALSE)} and \code{setompnested(enable = TRUE)}
#' as described here: https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
#'
#' @param enable A single logical indicating whether or not to enable MKL
#'   dynamic. [default = FALSE]
#'
#' @return None
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function depends on:
#' \code{\link{load_setthreads()}}
#' And is related to:
#' \code{\link{setompnested()}}
#' \code{\link{setmklthreads()}}
#' \code{\link{setompthreads()}}
#'
#' @examples
#' \dontrun{
#' setmkldynamic(enable = FALSE) # disables MKL dynamic
#' }
#' @useDynLib LBDCore
#' @export
setmkldynamic <- function(enable = FALSE) {
  # Only logicals allowed for our only argument
  if (!is.logical(enable)) stop("Logical values only to enable/disable MKL dynamic...\nExiting!")

  # This shared library will only exist in a Singularity image
  if (is_lbd_singularity()) {
    # "setthreads.so" should already be loaded, but if it isn't, let's try
    # to load it again
    if (!"setthreads" %in% names(getLoadedDLLs())) load_setthreads()

    # The 'setthreads.so' built into early images did not have a 'setMKLdynamic'
    # function included. Still can get by without it in some instances, but
    # let's at least let the user know.
    if (!is.loaded("setMKLdynamic")) {
      message(paste0(
        "WARNING: 'setmkldynamic' unavailable in LBD image: '",
        Sys.getenv("SINGULARITY_NAME"), "'"
      ))
    } else {
      if (enable) {
        message("WARNING: Enabling MKL dynamic...")
        message("         May cause performance issues when used with OpenMP")
        message("         enabled programs.")
      }
      invisible(.C("setMKLdynamic", as.integer(enable)))
    }
  } else {
    message("WARNING: Not and LBD Singularity image. No MKL dynamic adjustment made.")
  }
}
