#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION
#' @param target_ss PARAM_DESCRIPTION
#' @param min_in_bin PARAM_DESCRIPTION, Default: 5
#' @param rand PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname quadtree_ct
#' @export
quadtree_ct <- function(xy, ss, target_ss, min_in_bin = 5, rand = T) {

  ## my hacked version. quadtree by sum of value at each point.
  ## is ss=1 for all points you get back to original quadtree


  ## this function quadtrees by sample size
  ## rand introduces some randomness to the "median"
  print(paste0("Aiming for ts between: ", target_ss, " and ", 2 * target_ss))
  d <- dim(xy)[2]

  quad2(xy = xy, i = 1, id = 1, ss = ss, target_ss = target_ss, min_in_bin = min_in_bin, rand = rand, d = d)
}
