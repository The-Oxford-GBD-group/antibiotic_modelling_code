## is_lbd_core_repo() --------------------------------------------------------->
#' @title
#' A function to detect if a directory path is the central lbd_core repo or
#' (likely) a fork of that repo.
#'
#' @description
#' \code{is_lbd_core_repo} returns TRUE/FALSE if final subdirectory is
#' 'lbd_core'
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' A function to detect if a directory path is the central lbd_core repo or
#' likely a fork of that repo. This is necessary for \code{mbg_setup} to know
#' whether or not to search only the 'mbg_central' subdirectory or not.
#'
#' @param path A path
#'
#' @return TRUE/FALSE
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#' \code{\link{mbg_setup}}
#'
#' @export
is_lbd_core_repo <- function(path) {
  dir_names <- strsplit(path, "/")[[1]]
  dir_names <- dir_names[which(dir_names != "")]
  if (dir_names[length(dir_names)] == "lbd_core") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
