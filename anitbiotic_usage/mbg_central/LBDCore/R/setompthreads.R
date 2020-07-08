## setompthreads() ------------------------------------------------------------>
#' @title Sets number of OMP threads
#'
#' @description
#' \code{setompthreads()} Uses a function in the "setthreads.so" shared library
#' built into LBD Singularity images to set the number of OMP threads to a user
#' defined value
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Uses a function in the "setthreads.so" shared library built into LBD
#' Singularity images to set the number of OMP threads to a user defined value.
#' The shared library should exist in the LBD Singularity image and should have
#' already been loaded by \code{mbg_setup()}. This function checks to make sure
#' that it is loaded and if not, attempts to use \code{load_setthreads()} to do
#' so. A warning is generated if this is run outside of an LBD Singularity image
#' and no thread adjustment is done.
#'
#' @param threads A single value indicating the number of threads to use
#'   [default = 1]
#'
#' @return None
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function depends on:
#' \code{\link{load_setthreads()}}
#' \code{\link{is_integer()}}
#' Is used by:
#' \code{\link{set_serial_threads()}}
#' \code{\link{set_original_threads()}}
#' And is related to:
#' \code{\link{get_omp_threads()}}
#' MKL thread setting is done with \code{\link{setmklthreads}}
#'
#' @examples
#' \dontrun{
#' setompthreads(2) # sets the OMP threads to 2
#' }
#' 
#' @useDynLib LBDCore
#'
#' @export
setompthreads <- function(threads = 1) {
  # This shared library will only exist in a Singularity image
  if (is_lbd_singularity()) {
    # "setthreads.so" should already be loaded, but if it isn't, let's try to
    # load it again
    if (!"setthreads" %in% names(getLoadedDLLs())) load_setthreads()

    # Verify that only an integer has been passed in for the number of threads
    if (!is_integer(threads) | threads <= 0) {
      stop("Integer thread values >= 0 only...\nExiting!")
    } else {
      if (threads > 1) {
        message("WARNING: Enabling multi-threaded OMP...")
        message("         Use caution not to oversubscribe the node or when using")
        message("         with forked processes like 'mclapply()'")
      }
    }
    invisible(.C("setOMPthreads", as.integer(threads)))
  } else {
    message("WARNING: Not an LBD Singularity image. No OMP thread adjustment made.")
  }
}
