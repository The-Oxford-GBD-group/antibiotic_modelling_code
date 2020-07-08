# Some functions to load R packages from specific directories and R functions
# from one or more repo directories
#
## is_singularity() ----------------------------------------------------------->
#' @title Tests if within a Singularity container
#'
#' @description
#' \code{is_singularity} returns TRUE if within a Singularity container, FALSE
#' otherwise.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' This function can determine if it is being run in a Singularity container.
#' If the 'image_dir' argument is left blank, it will return TRUE if it is
#' within any Singularity container. If a path is provided for 'image_dir', this
#' function will test if the container was spun up from an image that lives in
#' the specified directory. This will help us determine if the container was
#' spun up from a specific team's image, which can be important to distinguish
#' where load packages from or if one should attempt to fit a model with a
#' special package like TMB.
#'
#' @param image_dir A path to a directory containing Singularity images
#'   [default = ""]. Must be a valid path.
#'
#' @return TRUE/FALSE
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#'   \code{\link{is_lbd_singularity}}
#'
#' @examples
#' \dontrun{
#' # return TRUE if in any Singularity container / FALSE otherwise
#' is_singularity()
#' # TRUE only if container is spun up from image in "/share/singularity-images/lbd"
#' is_singularity(image_dir = "/share/singularity-images/lbd")
#' # Will \code{stop()} because an invalid path was supplied
#' is_singularity(image_dir = "/path/to/nowhere")
#' }
#' @export
is_singularity <- function(image_dir = "") {
  if ("SINGULARITY_NAME" %in% names(Sys.getenv())) {
    # if we only want to test if this is an image or not
    if (image_dir == "") {
      return(TRUE)
    } # if this is a container and the 'group_image' is not blank, find out if the
    # container was spun up from an image in the specified directory
    else {
      if (!dir.exists(image_dir)) stop(paste0("Invalid file path: '", image_dir, "'...\nExiting!"))
      image_name <- Sys.getenv("SINGULARITY_NAME")
      if (file.exists(paste0(image_dir, "/", image_name))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } else {
    return(FALSE)
  }
}
