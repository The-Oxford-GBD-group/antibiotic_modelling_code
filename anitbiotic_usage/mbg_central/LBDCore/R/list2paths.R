#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname list2paths
#' @export
list2paths <- function(x) {

  # get node and branch names
  nodes <- unlist(x, use.names = FALSE)
  branches <- unlist(getBranches(x),
    use.names = FALSE
  )

  # append the node name
  paths <- file.path(branches, nodes)

  # remove extra slashes
  paths <- gsub("/+", "/", paths)

  paths
}
