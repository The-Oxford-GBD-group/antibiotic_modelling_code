#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dbname PARAM_DESCRIPTION
#' @param host PARAM_DESCRIPTION
#' @param query PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname run_query
#' @export
run_query <- function(dbname, host, query) {
  con <- get_con(dbname, host)
  con %>% tbl(sql(query)) %>% collect(n = Inf) %>% data.table()
}
