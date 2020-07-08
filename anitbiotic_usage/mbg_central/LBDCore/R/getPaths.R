#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pathfile PARAM_DESCRIPTION, Default: '~/Z/ABRAID/prevalence modelling/under five mortality/paths_for_nick.csv'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getPaths
#' @export
getPaths <- function(pathfile = "~/Z/ABRAID/prevalence modelling/under five mortality/paths_for_nick.csv") {
  # get file paths for the key datasets
  # path points to a csv file containing named paths, the function returns a dataframe
  # of named filepaths
  paths <- read.csv(pathfile,
    row.names = 1
  )
  data.frame(t(paths),
    stringsAsFactors = FALSE
  )
}
