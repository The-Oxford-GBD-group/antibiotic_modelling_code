## get_omp_threads() ---------------------------------------------------------->
#' @title Finds number of threads set for OpenMP operations
#'
#' @description
#' \code{get_omp_threads()} Uses environmental variable "OMP_NUM_THREADS" used
#' in LBD Singualirty images to determine how many threads have been assigned
#' for OpenMP operations.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' The OpenMP function \code{omp_get_num_threads()} will report how many threads
#' have been set for OpenMP operations. Unfortunately, there is no
#' "mkl_get_num_threads()" function in the MKL library, so we have to rely on
#' our OMP_NUM_THREADS environmental variable to find out how many threads have
#' been assigned for OpenMP operations as we do for MKL. Fortunately, we can
#' guarantee that OMP_NUM_THREADS has been set in LBD Singularity containers
#' spun up by either "shell_sing.sh" or "singR.sh".
#'
#' @return number of threads assigned to OpenMP
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function is used by:
#' \code{\link{get_total_threads()}}
#'
#' @export
get_omp_threads <- function() {
  if (is_lbd_singularity() & !is_rstudio()) {
    if ("OMP_NUM_THREADS" %in% names(Sys.getenv())) {
      omp_threads <- as.integer(Sys.getenv("OMP_NUM_THREADS"))
    } else {
      message("Warning: Unexpectedly, no OMP_NUM_THREADS environmental set in LBD image.")
      omp_threads <- 1
    }
    return(omp_threads)
  } else {
    return(1)
  }
}
