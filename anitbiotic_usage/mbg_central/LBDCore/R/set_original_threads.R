## set_original_threads() ------------------------------------------------------->
#' @title Sets MKL/OMP threads back to original setting
#'
#' @description
#' \code{set_original_threads()} Uses functions in the "setthreads.so" shared
#' library built into LBD Singularity images to set MKL/OMP back to an original
#' setting.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Uses functions in the "setthreads.so" shared library built into LBD
#' Singularity images to set MKL/OMP threads back to an original setting. The
#' shared library should exist in the LBD Singularity image and should have
#' already been loaded by \code{mbg_setup()}. This function checks to make sure
#' that it is loaded and if not, attempts to use \code{load_setthreads()} to do
#' so. A warning is generated if this is run outside of an LBD Singularity image
#' and no thread adjustment is done.
#'
#' This function is meant to be used after \code{set_threads_serial()} was used
#' to set MKL/OMP operations to serial before a call to a process that forks
#' (like \code{mclapply()}) which is known to have issues with threaded
#' processes. This function will try and retrieve the original thread settings
#' from the MKL_NUM_THREADS or OMP_NUM_THREADS environmental variable and again
#' set the threads back to those values. If those environmentals are not set,
#' it sets them to 1.
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function depends on:
#' \code{\link{load_setthreads}}
#' and is usually coupled with:
#' \code{\link{set_serial_threads}}
#'
#' @examples
#' \dontrun{
#' set_serial_threads()
#' mclapply(...)
#' set_original_threads()
#' }
#' 
#' @useDynLib LBDCore
#'
#' @export
set_original_threads <- function() {
  # This shared library will only exist in a Singularity image
  if (is_lbd_singularity()) {
    # "setthreads.so" should already be loaded, but if it isn't, let's try
    # to load it again
    if (!"setthreads" %in% names(getLoadedDLLs())) load_setthreads()

    # See if there was an MKL setting based on environmental originally
    if ("MKL_NUM_THREADS" %in% names(Sys.getenv())) {
      message("Setting MKL threads back to original setting")
      mkl_threads <- as.integer(Sys.getenv("MKL_NUM_THREADS"))
    } else {
      message("WARNING: No MKL_NUM_THREADS environmental set in LBD image. Setting MKL to serial.")
      mkl_threads <- 1
    }
    invisible(.C("setMKLthreads", as.integer(mkl_threads)))

    # See if there was an OMP setting based on environmental originally
    if ("OMP_NUM_THREADS" %in% names(Sys.getenv())) {
      message("Setting OMP threads back to original setting")
      omp_threads <- as.integer(Sys.getenv("OMP_NUM_THREADS"))
    } else {
      message("WARNING: No OMP_NUM_THREADS environmental set in LBD image. Setting OMP to serial.")
      omp_threads <- 1
    }
    invisible(.C("setOMPthreads", as.integer(omp_threads)))
  } else {
    message("WARNING: Not and LBD Singularity image. No thread adjustment made.")
  }
}
