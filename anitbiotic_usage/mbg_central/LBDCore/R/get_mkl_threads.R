## get_mkl_threads() ---------------------------------------------------------->
#' @title Finds number of threads set for MKL operations
#'
#' @description
#' \code{get_mkl_threads()} Uses environmental variable "MKL_NUM_THREADS" used
#' in LBD Singualirty images to determine how many threads have been assigned
#' for MKL operations.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' The OpenMP function \code{omp_get_num_threads()} will report how many threads
#' have been set for OpenMP operations. Unfortunately, there is no
#' "mkl_get_num_threads()" function in the MKL library, so we have to rely on
#' our MKL_NUM_THREADS environmental variable to find out how many threads have
#' been assigned for MKL operations. Fortunately, we can guarantee that
#' MKL_NUM_THREADS has been set in LBD Singularity containers spun up by either
#' "shell_sing.sh" or "singR.sh".
#'
#' @return number of threads assigned to MKL
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function is used by:
#' \code{\link{get_total_threads()}}
#'
#' @export
get_mkl_threads <- function() {
  if (is_lbd_singularity() & !is_rstudio()) {
    if ("MKL_NUM_THREADS" %in% names(Sys.getenv())) {
      mkl_threads <- as.integer(Sys.getenv("MKL_NUM_THREADS"))
    } else {
      message("Warning: Unexpectedly, no MKL_NUM_THREADS environmental set in LBD image.")
      mkl_threads <- 1
    }
    return(mkl_threads)
  } else {
    return(1)
  }
}
