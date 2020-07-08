#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param qsubs_to_make PARAM_DESCRIPTION
#' @param stderr_log PARAM_DESCRIPTION
#' @param stdout_log PARAM_DESCRIPTION
#' @param project PARAM_DESCRIPTION
#' @param resources PARAM_DESCRIPTION
#' @param singularity_str PARAM_DESCRIPTION
#' @param queue PARAM_DESCRIPTION. Default: NULL.
#' @param slots PARAM_DESCRIPTION
#' @param shell PARAM_DESCRIPTION
#' @param code PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param pop_measure PARAM_DESCRIPTION
#' @param overwrite PARAM_DESCRIPTION
#' @param corerepo PARAM_DESCRIPTION
#' @param raking_shapefile_version PARAM_DESCRIPTION
#' @param modeling_shapefile_version PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_qsubs_aggregation
#' @export
make_qsubs_aggregation <- function(qsubs_to_make,
                                   stderr_log,
                                   stdout_log,
                                   project,
                                   resources,
                                   singularity_str,
                                   queue = NULL,
                                   priority = 0,
                                   slots,
                                   shell,
                                   code,
                                   indicator,
                                   indicator_group,
                                   run_date,
                                   pop_measure,
                                   overwrite,
                                   corerepo,
                                   raking_shapefile_version,
                                   modeling_shapefile_version) {
  qsubs <- c()
  for (i in 1:nrow(qsubs_to_make)) {
    region <- qsubs_to_make[i, 1]
    holdout <- qsubs_to_make[i, 2]
    age <- qsubs_to_make[i, 3]
    rake <- qsubs_to_make[i, 4]
    shapefile_version <- if (rake) raking_shapefile_version else modeling_shapefile_version
    job_name <- paste(indicator, region, "aggregate", sep = "_")

    qsub <- generate_qsub_command(
      stderr_log = stderr_log,
      stdout_log = stdout_log,
      project = project,
      resources = resources,
      job_name = job_name,
      singularity_str = singularity_str,
      queue = queue,
      cores = slots,
      priority = priority,
      shell, code,
      indicator,
      indicator_group,
      run_date,
      rake,
      pop_measure,
      overwrite,
      age,
      holdout,
      region,
      corerepo,
      shapefile_version
    )
    qsubs <- c(qsubs, qsub)
  }
  return(qsubs)
}
