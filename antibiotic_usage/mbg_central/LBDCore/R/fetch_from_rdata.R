#' @title fetch_from_rdata
#' @description This pulls out pieces of an Rdata object
#'
#' @param file_location where to look for the Rdata object
#' @param item_name the item to pull from the Rdata object
#' @param use_grep Weather or not to use grep to look for similar file names
#'
#' @return loads the specific item from an Rdata object
#' @export
fetch_from_rdata <- function(file_location, item_name, use_grep = F) {
  load(file_location)
  if (use_grep) {
    return(mget(grep(item_name, ls(), value = T)))
  } else {
    return(get(item_name))
  }
}
