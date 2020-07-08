#' @title Push to global environment
#' @description Takes a list and assigns its variables to the global scope
#' @param x a \code{named} list
#' @return None
#' @examples
#' x <- list(a = 1, b = "C")
#' push_to_global_env(x)
#' "a" %in% ls()
#' "b" %in% ls()
#' @rdname push_to_global_env
#' @export
push_to_global_env <- function(x) {

  ## Make sure the input is a list
  stopifnot(class(x) == "list")

  ## Make sure the input is a *named* list
  stopifnot(!("" %in% names(x)))
  stopifnot(!(NA %in% names(x)))

  ## Assign to global environment
  for (outvar in names(x)) {
    assign(outvar, x[[outvar]], envir = globalenv())
  }
}
