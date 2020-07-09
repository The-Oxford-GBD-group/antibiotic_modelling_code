####################################################################################################################################################
# 																	 Pulls
####################################################################################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_ages
#' @export
get_ages <- function() {
  dbname <- "shared"
  host <- "modeling-cod-db.ihme.washington.edu"
  query <- "SELECT * FROM shared.age_group"
  run_query(dbname, host, query)
}
