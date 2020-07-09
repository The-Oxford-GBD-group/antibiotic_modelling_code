#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param pop_measure PARAM_DESCRIPTION
#' @param repo PARAM_DESCRIPTION, Default: core_repo
#' @param log_dir PARAM_DESCRIPTION
#' @param target_type PARAM_DESCRIPTION, Default: 'greater'
#' @param target PARAM_DESCRIPTION, Default: 0.5
#' @param geo_nodes PARAM_DESCRIPTION, Default: FALSE
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname submit_validation_report
#' @export
submit_validation_report <- function(reg, indicator, indicator_group,
                                     run_date, pop_measure,
                                     repo = core_repo, log_dir,
                                     target_type = "greater",
                                     target = 0.5, geo_nodes = FALSE,
                                     shapefile_version = "current") {
  dir.create(log_dir)
  dir.create(paste0(log_dir, "/errors"))
  dir.create(paste0(log_dir, "/output"))

  ## do we want to submit to geo nodes? if so, there are a few tweaks
  if (geo_nodes == TRUE) {
    proj <- "proj_geo_nodes" ## correct proj for geos nodes
    node.flag <- "-l geos_node=TRUE" ## send the job to geo nodes
  } else {
    proj <- "proj_geospatial"
    node.flag <- ""
  }
  shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_cluster.sh")

  qsub <- paste0(
    "qsub -e ", log_dir, "/errors -o ", log_dir,
    "/output -cwd -pe multi_slot 20 -P ", proj, " ",
    node.flag, " -N ", reg, "_val ", shell, " ", repo,
    "/mbg_central/run_validation_report.R ", indicator,
    " ", indicator_group, " ", run_date, " ",
    pop_measure, " ", repo, " ", reg, " ", target_type,
    " ", target, " ", shapefile_version, " fin"
  )
  system(qsub)
  return(NULL)
}
