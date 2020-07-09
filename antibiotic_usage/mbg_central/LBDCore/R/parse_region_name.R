#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @param regions PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname parse_region_name
#' @export
parse_region_name <- function(i, regions) {
  region_start <- grep("bin", regions[[1]]) + 1
  this_reg_name <- regions[[i]][region_start]
  ## Handle custom regions that may have an '_' in them.
  for (add in (region_start + 1):length(regions[[i]])) {
    if (!grepl("RData", regions[[i]][add])) this_reg_name <- paste0(this_reg_name, "_", regions[[i]][add])
  }
  return(this_reg_name)
}
