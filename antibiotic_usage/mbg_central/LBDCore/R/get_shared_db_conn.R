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
#' @seealso
#'  \code{\link[RMySQL]{character(0)}},\code{\link[RMySQL]{MySQLDriver-class}},\code{\link[RMySQL]{constants}}
#' @rdname get_shared_db_conn
#' @export
#' @importFrom RMySQL dbConnect MySQL CLIENT_MULTI_RESULTS
get_shared_db_conn <- function() {
  conn <- RMySQL::dbConnect(RMySQL::MySQL(),
    user = "dbview",
    password = "E3QNSLvQTRJm",
    host = "cod-db-p01.ihme.washington.edu",
    client.flag = RMySQL::CLIENT_MULTI_RESULTS
  )
  return(conn)
}
