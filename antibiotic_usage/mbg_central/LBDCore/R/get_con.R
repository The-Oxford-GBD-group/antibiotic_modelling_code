#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dbname PARAM_DESCRIPTION
#' @param host PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_con
#' @export
get_con <- function(dbname, host) {
  con <- suppressWarnings(src_mysql(dbname = dbname, host = host, user = "dbview", password = "E3QNSLvQTRJm"))
  return(con)
}
