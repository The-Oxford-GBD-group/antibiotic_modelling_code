#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param vars PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname collapse_list
#' @export
collapse_list <- function(df, vars) {

  ## Detect meta
  meta <- setdiff(names(df), vars)

  ## Binary for whether or not variable exists and is not completely missing
  out.vars <- sapply(vars, function(x) ifelse(x %in% names(df) & nrow(df[!is.na(x)]) > 0, 1, 0)) %>% t() %>% data.table()

  return(cbind(out.meta, out.vars))
}
