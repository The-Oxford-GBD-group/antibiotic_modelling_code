## setompnested() ------------------------------------------------------------>
#' @title Enables OpenMP nested parallelism
#'
#' @description
#' \code{setompnested()} Uses a function in the "setthreads.so" shared library
#' built into LBD Singularity images to enable/disable OpenMP nested
#' parallelism.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Uses a function in the "setthreads.so" shared library built into LBD
#' Singularity images to enable/disable OpenMP nested parallelism. Since many of
#' the packages we use in R rely on both OpenMP and MKL, where an OpenMP
#' parallel region can call the MKL and threading is enabled on both, we want to
#' enable OMP nested parallelism, which is done by default here (!= 0 is a TRUE,
#' i.e. requests enabling OMP nested parallelism, see:
#' https://msdn.microsoft.com/en-us/library/sk3zt8e1.aspx)
#' The shared library should exist in the LBD Singularity image and should have
#' already been loaded by \code{mbg_setup()}. This function checks to make sure
#' that it is loaded and if not, attempts to use \code{load_setthreads()} to do
#' so. A warning is generated if this is run outside of an LBD Singularity image
#' and no OMP nesting is done. This function is normally used along with
#' \code{setmkldynamic()} as follows:
#' \code{setmkldynamic(enable = FALSE)} and \code{setompnested(enable = TRUE)}
#' as described here: https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
#'
#' @param enable A single logical indicating whether or not to enable OpenMP
#'   nested parallelism [default = TRUE]
#'
#' @return None
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function depends on:
#' \code{\link{load_setthreads()}}
#' And is related to:
#' \code{\link{setmkldynamic()}}
#' \code{\link{setmklthreads()}}
#' \code{\link{setompthreads()}}
#'
#' @examples
#' \dontrun{
#' setompnested(enable = TRUE) # disables OMP nesting
#' }
#' 
#' @useDynLib LBDCore
#' @export
setompnested <- function(enable = TRUE) {
  # Only logicals allowed for our only argument
  if (!is.logical(enable)) stop("Logical values only to enable/disable OpenMP nested parallelism...\nExiting!")

  # This shared library will only exist in a Singularity image
  if (is_lbd_singularity()) {
    # "setthreads.so" should already be loaded, but if it isn't, let's try
    # to load it again
    if (!"setthreads" %in% names(getLoadedDLLs())) load_setthreads()

    # The 'setthreads.so' built into early images did not have a 'setOMPnested'
    # function included. Still can get by without it in some instances, but
    # let's at least let the user know.
    if (!is.loaded("setOMPnested")) {
      message(paste0(
        "WARNING: 'setompnested' unavailable in LBD image: '",
        Sys.getenv("SINGULARITY_NAME"), "'"
      ))
    } else {
      if (!enable) {
        message("WARNING: Disabling OMP nesting...")
        message("         May cause performance issues when used with MKL")
        message("         enabled programs.")
      }
      invisible(.C("setOMPnested", as.integer(enable)))
    }
  } else {
    message("WARNING: Not and LBD Singularity image. No OpenMP nested parallelism adjustment made.")
  }
}
