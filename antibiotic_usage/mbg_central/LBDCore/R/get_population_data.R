
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param simple_raster PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_population_data
#' @export
get_population_data <- function(simple_raster) {
  #################################################################################
  ### Pull population data and return as a raster
  ## Inputs:
  # simple_raster:
  ## Outputs:
  ## Notes:
  # Population data in a temporary location, Lucas to find permanent home for it
  # Later will need to be explicit about years (for now everyone is doing 2000-2015, 4 yr intervals)
  #################################################################################


  # load population raster
  # RB 27SEPT: Note this is a temporary location, and is only Africa so updates will be necessary
  root <- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/")
  pop <- brick(sprintf("%stemp/geospatial/U5M_africa/data/raw/covariates/new_20160421/pop_stack.tif", root))

  # make sure population is cropped and extented to same as simple_raster
  # this is important, otherwise IDX wont work.
  if (!is.null(simple_raster)) {
    pop <- mask(crop(pop, simple_raster), simple_raster)
    extent(pop) <- extent(simple_raster)
  }
  return(pop)
}
