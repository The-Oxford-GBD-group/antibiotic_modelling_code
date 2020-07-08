#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_proper_name PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param pop_measure PARAM_DESCRIPTION
#' @param repo PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @param nperiod PARAM_DESCRIPTION, Default: 16
#' @param log_dir PARAM_DESCRIPTION
#' @param geo_nodes PARAM_DESCRIPTION, Default: FALSE
#' @param shapefile_verison PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname submit_validation_report_pub
#' @export
submit_validation_report_pub <- function(indicator, indicator_proper_name, indicator_group, run_date, pop_measure, repo, region,
                                         nperiod = 16, log_dir, geo_nodes = FALSE, shapefile_verison = "current") {
  dir.create(log_dir)
  dir.create(paste0(log_dir, "/errors"))
  dir.create(paste0(log_dir, "/output"))



  ## do we want to submit to geo nodes? if so, there are a few tweaks
  if (geo_nodes == TRUE) {
    shell <- "r_shell_geos.sh" ## for the correct path to R
    proj <- "proj_geo_nodes" ## correct proj for geos nodes
    node.flag <- "-l geos_node=TRUE" ## send the job to geo nodes
    shell <- sprintf("%s/mbg_central/share_scripts/shell_geos.sh", repo)
  } else {
    proj <- "proj_geospatial"
    node.flag <- ""
    shell <- sprintf("%s/mbg_central/share_scripts/shell_prod.sh", repo)
  }

  # because the cluster is dumb
  indicator_proper_name <- gsub(" ", "###", indicator_proper_name, fixed = T)

  args <- paste(indicator, indicator_proper_name, indicator_group, run_date, pop_measure, repo, region, nperiod, shapefile_version, "fin")

  qsub <- paste0("qsub -e ", log_dir, "/errors -o ", log_dir, "/output -pe multi_slot 20 -P ", proj, " ", node.flag, " -N ", region, "_val ", shell, " ", repo, "/mbg_central/run_validation_report_pub.R ", args)
  system(qsub)
  return(NULL)
}
