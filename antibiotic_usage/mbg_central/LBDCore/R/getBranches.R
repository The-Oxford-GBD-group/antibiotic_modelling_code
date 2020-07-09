#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getBranches
#' @export
getBranches <- function(x, parent = "") {
  # loop through levels of a tree-like list,
  # flattening names of levels into
  # slash-separated character vector

  # get element names
  names <- names(x)

  # if unnamed, repeat the parent names
  if (is.null(names)) {
    names <- rep.int(parent, length(x))
  } else {
    names <- paste0(parent, names)
  }

  # if there are more branches ahead
  if (is.list(x)) {

    # add a slash
    names <- paste0(names, "/")

    # define a function to loop through them
    getTwigs <- function(i) {
      getBranches(x[[i]], names[i])
    }

    # get the names from these
    names <- lapply(1:length(x), getTwigs)
  }

  return(names)
}
