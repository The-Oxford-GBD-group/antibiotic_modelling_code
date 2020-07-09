#' @title Create an empty marker file for tracking start or end of jobs
#' 
#' @note If \code{JOB_ID} is not found in the environment variable, and
#' the R session is not interactive, then this function will break.
#' @param type Either \code{"start"} or \code{"end"}.
#' @param tmpdir Temporary folder to store job markers in.
#' Defaults to a folder in your home directory.
#' 
#' @export
mbg_job_marker <- function(type, tmpdir = "~/.mbgjob") {
  
  if(!(type %in% c("start", "end")) ) {
    stop("'type' must be one of 'start' or 'end'")
  }
  
  # Get job ID
  job_id <- Sys.getenv("JOB_ID")
  if((job_id == "" | is.null(job_id)) & (!interactive()) ) {
    stop("The environment variable job_id not found in non-interactive session")
  }
  
  # If interactive, then:
  if((job_id == "" | is.null(job_id)) & (interactive()) ) {
   job_id <- "inter"
  }
  
  # Create temp folder
  if(!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
  
  # Create temp (empty) file
  write(NULL, file = paste0(tmpdir, "/", job_id, "_", type))
  
  # Finally, if we're writing out the "end" type, then we need to erase
  # the start file
  if(type == "end") {
    file.remove(paste0(tmpdir, "/", job_id, "_start"))
  }
  
}
