#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param query PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[DBI]{dbGetQuery}},\code{\link[DBI]{character(0)}}
#'  \code{\link[RMySQL]{dbNextResult}}
#' @rdname run_sql_query
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom RMySQL dbMoreResults dbNextResult
run_sql_query <- function(conn, query) {
  rows <- DBI::dbGetQuery(conn, query)
  # recommended by "R Cookbook" as a bengin defensive measure, in case MySQL
  # returns an additional result set with status information
  if (RMySQL::dbMoreResults(conn)) RMySQL::dbNextResult(conn)
  return(rows)
}
