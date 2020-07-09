#' @title Determines how many threads to use in forked applications
#'
#' @description
#' \code{get_max_forked_threads()} Determines how many threads to give forked
#' applications (like \code{mclapply()}).
#'
#' @author Ian M. Davis, \email{imdavis\@uw.edu}
#'
#' @details
#' When using \code{mclapply()} we should only parallelize over the smaller of
#' the two values: the number of total cores we have available or the number of
#' objects being parallelized over. By first determining this, we can make sure
#' not to use more cores than have been allocated for the job or spawn a bunch
#' of forked \code{mclapply()} processes unnecessarily that then just sleep. In
#' other words, it makes no sense to launch more processes than are being
#' than you have operations to run in parallel, or launch more processes than
#' there are cores available for your job.
#'
#' In order for this to work properly, we assume that OpenMP and MKL operations
#' have been set to serial by \code{set_serial_threads()} before a forked
#' operation is launched, which it should be to avoid hanging the operation.
#'
#' @param threads If NULL, \code{get_total_threads()} is used to determine
#'   how many cores are available for the process. This assumes that all other
#'   multi-core applications have been set to serial with
#'   \code{set_serial_threads()}. An integer value of threads may also be passed
#'   in [default = NULL].
#' @param nobjs Number of objects the forked process will parallelize over
#'
#' @return number of threads to use in forked operation
#'
#'
#' @family Mutlti-threading Functions
#'
#' @seealso This function depends on:
#' \code{\link{get_total_threads()}}
#' Related functions:
#' \code{\link{set_serial_threads()}}
#' \code{\link{set_original_threads()}}
#'
#' @examples
#' \dontrun{
#' ## Set to serial operation, use \code{get_max_forked_threads()} to
#' ## determine how many threads to use, run the forked operation, then set the
#' ## threads back to the original setting:
#' set_serial_threads()
#' cores <- get_max_forked_threads(nobjs = length(folds))
#' model <- mclapply(folds, ...)
#' set_original_threads()
#' }
#' 
#' @export
get_max_forked_threads <- function(threads = NULL, nobjs) {
  if (is.null(threads)) threads <- get_total_threads()
  if (!is_integer(threads) | threads <= 0) {
    stop("Integer thread values >= 0 only...\nExiting!")
  }
  return(min(threads, nobjs))
}
