#' @title Which Singularity image to use
#'
#' @description \code{get_singularity} determines which Singularity image to use. The
#' default is the 'default' keyword. Image names without the full path to the
#' file defined are assumed to exist as the default location:
#'    \code{/share/singularity-images/lbd}
#' In either case it will test to make sure the file exists and exit if it does
#' not. The default image is hardcoded into the shell script used to launch
#' Singularity containers:
#'   \code{lbd_core/mbg_central/share_scripts/shell_sing.sh}
#'
#' @param image A string that defines which Singularity image to launch
#'   [default = 'default']. If the 'default' keyword is passed in or left blank,
#'   the default keyword will be returned. Either the full path to the image
#'   may be provided or only the Singularity image name. In the latter case,
#'   the image is assumed to live in the default image location:
#'   \code{/share/singularity-images/lbd.}
#'
#' @return When image = 'default', 'default' is returned. When this keyword is
#'   passed to the shell_sing.sh script through `qsub` it will use the default
#'   Singularity image hardcoded into it. Otherwise, if the function is
#'   successful at verifying that the Singularity image file specified exists,
#'   it will return the full path to that image.
#'
#' @seealso This function is used by:
#'   \code{\link{parallelize}}
#'   \code{\link{make_qsub}}
#'   \code{\link{make_qsub_share}}
#'   \code{\link{make_qsub_postest}}
#'   \code{\link{submit_aggregation_script}}
#'
#' @export
get_singularity <- function(image = "default") {
  if (image == "default") { # use default image
    sing_image <- "default"
  } else if (grepl("/", image)) { # user supplied path to image
    sing_image <- image
  } else { # image at default location
    sing_image <- paste0("/share/singularity-images/lbd/", image)
  }
  # If something other than the default image is being used, let's make sure
  # the image file actually exists:
  if (!sing_image == "default" & !file.exists(sing_image)) {
    stop(paste0("Could not locate Singularity image: ", sing_image))
  }
  return(sing_image)
}
