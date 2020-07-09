#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param type PARAM_DESCRIPTION, Default: 'closure'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname lstype
#' @export
lstype <- function(type = "closure") {
  inlist <- ls(.GlobalEnv)
  if (type == "function") type <- "closure"
  typelist <- sapply(sapply(inlist, get), typeof)
  return(names(typelist[typelist == type]))
}
