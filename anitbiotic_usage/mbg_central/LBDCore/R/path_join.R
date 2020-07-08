#' @title Path joiner
#'
#' @description join values together using filesystem path separator.
#'
#' @param ... values to join.
#'
#' @return path built from arguments.
#'
#' @export
path_join <- function(...) {
  result <- paste(..., sep = .Platform$file.sep)
  if (length(result) == 0) stop("no arguments provided to path_join")
  return(result)
}
