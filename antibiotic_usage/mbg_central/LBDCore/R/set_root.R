#' @title Set root path for J drive
#'
#' @description A function to set the "root" based on the OS used.
#' @author Rebecca Stubbs
#'
#' @export
set_root <- function() {
  root <<- ifelse(Sys.info()[1] == "Windows", "J:/", "/home/j/") # Setting "root" as global variable
}
