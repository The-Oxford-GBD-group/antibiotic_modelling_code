#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cleanup_inla_scratch
#' @export
cleanup_inla_scratch <- function(run_date) {
  if (keep_inla_files == FALSE) {

    # Clean up INLA intermediate directories unless user has specified to keep them.
    inla_working_dir <- "/snfs1/temp/geospatial/inla_intermediate"
    inla_working_dirs <- list.dirs(inla_working_dir, recursive = FALSE)
    inla_working_dirs <- inla_working_dirs[grepl(run_date, inla_working_dirs)]
    for (inla_dir in inla_working_dirs) {
      unlink(inla_dir, recursive = TRUE)
    }
  }

  if (keep_inla_files == TRUE) {
    message("Keeping INLA intermediate files because keep_inla_files==TRUE in config.")
    message(paste0("Files stored here: /snfs1/temp/geospatial/inla_intermediate/inla_", run_date))
  }
}
