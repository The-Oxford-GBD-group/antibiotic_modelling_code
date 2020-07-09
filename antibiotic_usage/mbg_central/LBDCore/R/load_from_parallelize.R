#' @title Load from parallelize()
#' @description This function takes the two things passed in a qsub created by
#' \code{parallelize()} - the temp file name and which row in loopvars the current iteration
#' of the child script should load from - and loads the appropriate \code{save_objs} and
#' \code{expand_vars} from \code{parallelize()} into the environment of the child script.
#'
#' @note This is meant to be run from the \code{child script}; by default
#' both \code{fname} and \code{rownumber} should be loaded appropriately from
#' \code{commandArgs()}
#'
#' @param fname filename of the temp file created by \code{parallelize()}
#' @param rownumber which row of the `lv` object should this particular
#'                  instance of the child script load from
#' @return nothing; assigns objects to child script global environment.
#' @examples
#' # Note: this is within the CHILD SCRIPT, not the master script
#' 
#' # A good place to put this is right after you've sourced all the
#' # mbg_central function scripts.  Then simply run the
#' # function to set up your environment:
#' \dontrun{
#' load_from_parallelize()
#' }
#' 
#' @export
load_from_parallelize <- function(fname = as.character(commandArgs()[4]),
                                  rownumber = as.numeric(commandArgs()[5])) {
  message(paste0("fname: ", fname))
  message(paste0("rownumber: ", rownumber))
  tmp_dir <- "/share/geospatial/tmp/"

  # Load in the temporary object
  load(paste0(tmp_dir, fname, ".RData"), envir = .GlobalEnv, verbose = T)

  lv <- data.frame(lv)

  # Load loopvars
  this_lv <- lv[rownumber, -which(names(lv) == "jobname"), drop = FALSE]

  # Assign loopvars
  for (n in names(this_lv)) {
    assign(n, this_lv[1, which(names(this_lv) == n)], envir = .GlobalEnv)
    message(paste0(n, ": ", get(n)))
  }
}
