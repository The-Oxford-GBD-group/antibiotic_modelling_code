#' @title  Clean up path
#' @description Sometimes extra '/''s are added to file paths here and there when they are
#' constructed which makes it difficult to compare a file path (string) to
#' another. This function will clean up an existing file path removing
#' additional '/''s to make it possbile to compare them reliably
#'
#' @param messy_path a file path (character vector). This assumes that the path
#'    is constructed with one or more '/' as a separator and not '\'
#'
#' @return a 'clean' path with a single '/' as a separator and no trailing '/'
#'
#' @export
clean_path <- function(messy_path) {
  # convert all '/' to white space separating the diretory names between
  dir_names <- strsplit(messy_path, "/")[[1]]
  # paste together the diretory names with '/' separators ignoring empty space
  clean_path <- paste(dir_names[which(dir_names != "")], collapse = "/")
  # make sure the tack on any prepended '/' if the original path began with a '/'
  if (substr(messy_path, start = 1, stop = 1) == "/") clean_path <- paste0("/", clean_path)
  return(clean_path)
}
