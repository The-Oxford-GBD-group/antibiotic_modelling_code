#####################################################################################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param location_set_version_id PARAM_DESCRIPTION
#' @param china.fix PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_location_hierarchy
#' @export
get_location_hierarchy <- function(location_set_version_id, china.fix = FALSE) {
  dbname <- "shared"
  host <- "modeling-cod-db.ihme.washington.edu"
  ## Find active version id if not specified
  query <- paste0("SELECT * FROM shared.location_hierarchy_history WHERE location_set_version_id=", location_set_version_id)
  df <- suppressWarnings(run_query(dbname, host, query))

  ## China hierarchy fix
  if (china.fix) df <- china_hierarchy_fix(df)

  ## Create hierarchy
  hierarchy <- stringr::str_split_fixed(df$path_to_top_parent, ",", max(df$level) + 1) %>% data.table()
  hierarchy <- hierarchy[, lapply(.SD, as.numeric)]
  setnames(hierarchy, names(hierarchy), paste0("level_", seq(0, max(df$level))))
  df <- cbind(df, hierarchy)
  return(df)
}
