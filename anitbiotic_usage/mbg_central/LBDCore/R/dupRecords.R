#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param j PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname dupRecords
#' @export
dupRecords <- function(j) {
  record <- chunk[j, , drop = FALSE] # pull out the record
  # drop columns also in "points"
  record <- record[, !(colnames(record) %in% colnames(points)), with = FALSE]
  # faster than rbind / replicate
  record_dup <- cbind(record, points)

  return(record_dup)
}
