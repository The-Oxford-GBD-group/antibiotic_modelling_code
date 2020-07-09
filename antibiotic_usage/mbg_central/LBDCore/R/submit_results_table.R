#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param pop_measure PARAM_DESCRIPTION
#' @param repo PARAM_DESCRIPTION
#' @param log_dir PARAM_DESCRIPTION
#' @param baseline_year PARAM_DESCRIPTION
#' @param goal_threshold PARAM_DESCRIPTION
#' @param diarrhea_measure PARAM_DESCRIPTION, Default: ''
#' @param target_type PARAM_DESCRIPTION, Default: 'less'
#' @param geos_node PARAM_DESCRIPTION, Default: F
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname submit_results_table
#' @export
submit_results_table <- function(reg, indicator, indicator_group,
                                 run_date, pop_measure, repo, log_dir,
                                 baseline_year, goal_threshold,
                                 diarrhea_measure = "",
                                 target_type = "less", geos_node = F,
                                 shapefile_version = "current") {
  dir.create(log_dir)
  dir.create(paste0(log_dir, "/errors"))
  dir.create(paste0(log_dir, "/output"))

  shell.file <- ifelse(geos_node,
    "/mbg_central/r_shell_geos.sh",
    "/mbg_central/r_shell.sh"
  )

  proj.flag <- ifelse(geos_node,
    "-P proj_geo_nodes -l gn=TRUE",
    "-P proj_geospatial"
  )

  qsub <- paste0(
    "qsub -e ", log_dir, "/errors -o ", log_dir,
    "/output -cwd -pe multi_slot 30 ", proj.flag, " -N ",
    indicator, "_", reg, "_table ", repo, shell.file,
    " ", repo, "/mbg_central/run_results_table.R ",
    indicator, " ", indicator_group, " ", run_date, " ",
    pop_measure, " ", repo, " ", reg, " ", baseline_year,
    " ", goal_threshold, " ", diarrhea_measure, " ",
    target_type, " ", shapefile_version
  )

  system(qsub)

  return(NULL)
}
