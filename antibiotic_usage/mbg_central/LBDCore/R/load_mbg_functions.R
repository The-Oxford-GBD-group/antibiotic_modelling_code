## load_mbg_functions() ------------------------------------------------------->
#' @title Loads all of the other MBG functions from a specified repo (directory)
#'
#' @description
#' \code{load_mbg_functions} finds all of the *.R files with 'functions' in the
#' naming convention and sources them with a message.
#'
#' @author Ian M. Davis, \email{imdavis@uw.edu}
#'
#' @details
#' Makes sure that the provided repo directory exists and if so, attempts to
#' load scripts of R functions from within that directory. Note that this
#' function will *only* load R scripts with 'functions' in the naming convention
#' of the R script file name it finds after searching recursively from the
#' directory provided for 'repo'.
#'
#' @param repo Path to repository directory to attempt to load functions from
#'
#' @note There is an exception added to not pull from the 'testing' subdirectory
#'
#' @return None
#'
#' @family MBG setup functions
#'
#' @seealso This function is used by:
#' \code{\link{mbg_setup}}
#'
#' @export
load_mbg_functions <- function(repo) {

  ## Coerce relative to absolute path
  repo <- normalizePath(repo)

  message(paste0("\nChecking directory '", repo, "' for R scripts with 'functions' in the filename"))
  if (!dir.exists(repo)) stop(paste0("Directory '", repo, "' does not exist...\nExiting!"))
  functs <- list.files(
    path = repo,
    recursive = TRUE,
    pattern = "functions.R$",
    full.names = TRUE
  )

  ## Drop testing functions from being sourced
  functs <- functs[!grepl("testing", functs)]
  functs <- functs[!grepl("LBDCore", functs)]

  if (length(functs) == 0) {
    stop(paste0("Found no R scripts in '", repo, "' with 'functions' in the filename...\nExiting!"))
  } else {
    source_functions(functs)
  }
}
