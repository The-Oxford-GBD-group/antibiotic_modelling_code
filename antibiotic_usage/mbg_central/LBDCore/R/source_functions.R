## source_functions() --------------------------------------------------------------->
#' @title Sources a list of functions
#'
#' @description
#' \code{source_functions} sources a list of functions with
#' with complete path names
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' A standalone function that will source in functions from a provided character
#' vector of functions names with complete paths.
#'
#' @param functions Character vector of functions names including COMPLETE path
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#'   \code{\link{load_mbg_functions}}
#'
#' @examples
#' \dontrun{
#' mbg_functions <- c("setup.R", "mbg_functions.R", "misc_functions.R")
#' source_functions(paste0(
#'   "/share/code/geospatial/imdavis/repos/lbd_core/mbg_central/",
#'   mbg_functions
#' ))
#' }
#' 
#' @export
source_functions <- function(functions) {
  if (length(functions) == 0) {
    stop(paste0("'functions' argument empty...\nExiting!"))
  } else {
    for (funct in functions) {
      message(paste0("--> Loading: '", funct, "'"))
      source(funct)
    }
  }
}
