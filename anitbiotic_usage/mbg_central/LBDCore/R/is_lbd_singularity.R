## is_lbd_singularity() ------------------------------------------------------->
#' @title Tests if within LBD specific Singularity image
#'
#' @description
#' \code{is_lbd_singularity} convenience function to check if a Singularity
#' container was spun up specifically from a LBD image.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' This could be used in multiple places and only wanted to change the path
#' to where the Singularity images live (if it ever changes) in one place and
#' not many.
#'
#' @return TRUE/FALSE
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#' \code{\link{load_R_packages}}
#'
#' @export
is_lbd_singularity <- function() {
  is_singularity(image_dir = "/share/singularity-images/lbd") ||
    is_singularity(image_dir = "/share/singularity-images/lbd/test_deploy") ||
    is_singularity(image_dir = "/share/singularity-images/lbd/testing_INLA_builds")
}
