#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param jobids PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_qa_wrapper
#' @export
get_qa_wrapper <- function(jobids) {
  Sys.sleep(30) # Give a bit of time to make sure that exit status generated
  return(sapply(jobids, get_qacct_exit_status))
}
