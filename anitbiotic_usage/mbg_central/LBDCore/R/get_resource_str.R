#' @title Get resource string
#'
#' @description Compute a string of resources to request for a qsub command.
#'
#' @param resources named vector of resources.
#' @seealso \code{\link{get_resources}}
#'
#' @return String of resources
#' @export
get_resource_str <- function(resources) {
  if (length(resources) == 0) return("")
  paste0("-l ", names(resources), "=", resources, collapse = " ")
}
