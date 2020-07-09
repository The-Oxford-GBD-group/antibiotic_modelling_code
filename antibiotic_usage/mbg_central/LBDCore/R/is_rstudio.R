## is_rstudio() --------------------------------------------------------------->
#' @title Tests if within RStudio
#'
#' @description \code{is_rstudio} returns TRUE if within RStudio, FALSE otherwise.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' This function can tell if it is being run from within RStudio. If the
#' 'check_singularity' argument is FALSE (default) it will return TRUE if it is
#' in any RStudio session. If the argument is set to TRUE, it will return TRUE
#' only if 'singularity' is found within 'LD_LIBRARY_PATH'. Unfortunately, we
#' have to do it this way because rstudio-server seems to mask all of the normal
#' system environmental variables, so we can't check for SINGULARITY_NAME in the
#' environment as we do above in \code{is_singularity}.
#'
#' @param check_singularity Logical to check if RStudio is running in a
#'   Singularity container [default = FALSE]
#'
#' @return TRUE/FALSE
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#'   \code{\link{load_R_packages}}
#'
#' @examples
#' \dontrun{
#' # return TRUE if in RStudio / FALSE otherwise
#' is_rstudio()
#' # TRUE only if RStudio lives within a Singularity container
#' is_rstudio(check_singularity = FALSE)
#' }
#' 
#' @export
is_rstudio <- function(check_singularity = FALSE) {
  if (Sys.getenv("RSTUDIO") == 1) {
    if (!check_singularity) {
      return(TRUE)
    } else {
      if (grepl("singularity", Sys.getenv()["LD_LIBRARY_PATH"])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } else {
    return(FALSE)
  }
}
