#' @title Get output regions
#' @description This function takes in a directory where mbg modeling has stored
#' outputs (*cell_pred* objects) and infers the regions specirfied in
#' the model
#'
#' @description Determines modeling regions from written output dir objects
#'
#' @param in_dir directory path containing completed mbg cell_pred objects
#'
#' @return A vector string of region names
#' @export
get_output_regions <- function(in_dir) {
  return(unique(stringr::str_match(
    list.files(in_dir, pattern = paste0("_cell_draws_eb_")),
    "_cell_draws_[^_]+_[^_]+_(.*)_[0-9].RData"
  )[, 2]))
}
