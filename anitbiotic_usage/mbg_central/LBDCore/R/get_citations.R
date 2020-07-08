#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nids PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_citations
#' @export
get_citations <- function(nids) {
  dbname <- "shared"
  host <- "modeling-cod-db.ihme.washington.edu"
  query <- paste0("SELECT 
                  nid, field_citation_value, series_title
                  FROM
                  shared.mv_citation
                  WHERE nid in (", nids, ")")
  run_query(dbname, host, query)
}
