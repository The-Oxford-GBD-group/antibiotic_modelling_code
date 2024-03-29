## get_total_threads() -------------------------------------------------------->
#' @title Finds total number of threads available
#'
#' @description
#' \code{get_total_threads()} Uses functions \code{get_mkl_threads()} and
#' \code{get_omp_threads()} to determine total cores allocated to the job.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' In an LBD image, the total number of threads are broken up over OpenMP
#' operations (OMP_NUM_THREADS) and MKL operations (MKL_NUM_THREADS). Since we
#' set both OpenMP and MKL to serial for things like \code{mclapply()}, we can
#' at least try and parallelize as much as possible over forked process or other
#' parallel processes. We need to know how many total cores have been allocated
#' to the job to do this.
#'
#' @return total number of threads available for any operation
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function depends on:
#' \code{\link{get_mkl_threads()}}
#' \code{\link{get_total_threads()}}
#' This function is used by:
#' \code{\link{get_max_forked_threads()}}
#'
#' @examples
#' \dontrun{
#' ## Set to serial operation, use \code{get_total_threads()} to
#' ## determine how many total threads are available, run the parallel operation,
#' ## then set the threads back to the original setting:
#' set_serial_threads()
#' cores <- get_total_threads()
#' model <- fit_glmnet(...)
#' set_original_threads()
#' }
#' 
#' @export
get_total_threads <- function() {
  return(get_mkl_threads() * get_omp_threads())
}
