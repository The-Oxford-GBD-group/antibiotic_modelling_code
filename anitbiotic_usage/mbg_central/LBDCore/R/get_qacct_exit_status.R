#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param jobid PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_qacct_exit_status
#' @export
get_qacct_exit_status <- function(jobid) {
  qa <- system(paste0("qacct -j ", jobid), intern = T)
  qa <- str_match(qa, "exit_status\\s+([0-9]*)")[, 2]
  qa <- as.numeric(qa[!is.na(qa)])
  return(qa)
}
