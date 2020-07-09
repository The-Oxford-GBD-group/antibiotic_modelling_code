#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname idx
#' @export
idx <- function(...) {
  # convert strings or numerics to a sequential numeric index
  # pass any number of vectors of the same length, get a numeric ID for the
  # unique ones
  list <- list(...)

  # get their lengths
  lengths <- lapply(list, length)
  if (!isTRUE(do.call(all.equal, lengths))) {
    stop("vectors do not appear to have the same length")
  }
  # combine them
  x <- do.call(paste, list)
  # get numeric index
  match(x, unique(x))
}
