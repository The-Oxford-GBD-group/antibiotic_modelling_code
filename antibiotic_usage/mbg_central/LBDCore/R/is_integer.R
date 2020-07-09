## is_integer() ------------------------------------------------------------>
#' @title Tests if object is an integer
#'
#' @description
#' \code{is_integer()} Tests if object is an integer since \code{is.integer()}
#' does not.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' We need to be able to test if an object is an integer or not. Unfortunately
#' R's \code{is.integer()} does not actually tell you if an object is an integer
#' (`is.integer(5)` returns FALSE for example).  This function will return FALSE
#' if something that is not an integer is passed in, including something like a
#' string that R likes to coerce into a NA which when compared with something
#' else resulting in NA instead of TRUE or FALSE, ex.:
#' `x <- 'a'; x == as.integer(x)`
#'
#' @param i An object
#'
#' @return TRUE/FALSE
#'
#' @family MBG setup functions
#'
#' @seealso
#' \code{\link{setmklthreads}}
#' \code{\link{setompthreads}}
#'
#' @examples
#' \dontrun{
#' is_integer(1.2) # returns FALSE
#' is_integer(1.) # returns TRUE
#' is_integer(-1) # returns TRUE
#' is_integer("a") # returns FALSE
#' is_integer(5) # returns TRUE
#' is_integer(c(1, 1.2, 3)) # returns TRUE FALSE TRUE
#' is_integer(c("a", 1.2, 3)) # returns FALSE
#' }
#' @export
is_integer <- function(i) {
  # Give a FALSE if R gives a warning or an error. If R tries to coerce it isn't
  # an integer.
  tryCatch(i == as.integer(i),
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
}
