## set_serial_threads() ------------------------------------------------------->
#' @title Sets MKL/OMP threads to serial
#'
#' @description
#' \code{set_serial_threads()} Uses functions in the "setthreads.so" shared
#' library built into LBD Singularity images to set MKL/OMP threads to serial.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Uses functions in the "setthreads.so" shared ' library built into LBD
#' Singularity images to set MKL/OMP threads to serial. The shared library
#' should exist in ' the LBD Singularity image and should have already been
#' loaded by \code{mbg_setup()}. This function checks to make sure that it is
#' loaded and if not, attempts to use \code{load_setthreads()} to do so. A
#' warning is generated if this is run outside of an LBD Singularity image and
#' no thread adjustment is done.
#'
#' This can be used to set any multi-threaded operations to serial before a call
#' to a process that forks (like \code{mclapply()}) which is known to have
#' issues with threaded processes. Then \code{set_original_threads()} can be
#' used to set the threads back to their original setting.
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function depends on:
#' \code{\link{load_setthreads}}
#' and is usually coupled with:
#' \code{\link{set_original_threads}}
#'
#' @examples
#' \dontrun{
#' set_serial_threads()
#' mclapply(...)
#' set_original_threads()
#' }
#' 
#' @useDynLib LBDCore
#' @export
set_serial_threads <- function() {
  # This shared library will only exist in a Singularity image
  if (is_lbd_singularity()) {
    # "setthreads.so" should already be loaded, but if it isn't, let's try
    # to load it again
    if (!"setthreads" %in% names(getLoadedDLLs())) load_setthreads()
    # Give the user a message and set the threads to 1
    message("Setting MKL/OMP to serial")
    invisible(.C("setMKLthreads", as.integer(1)))
    invisible(.C("setOMPthreads", as.integer(1)))
  } else {
    message("WARNING: Not and LBD Singularity image. No thread adjustment made.")
  }
}
