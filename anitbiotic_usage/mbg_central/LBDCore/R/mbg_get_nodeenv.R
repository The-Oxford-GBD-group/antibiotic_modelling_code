#' @title Get image of node in the MBG DAG
#'
#' @description Get model data for each node in DAG
#'
#' @param node The name(s) of the node in the DAG
#'
#' @param indicator_group the indicator group e.g., "hiv" or "u5m".
#'
#' @param indicator the indicator e.g., "wasting_mod_b" or "male_circumcision".
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @param age the age for the model which uses this data.
#'
#' @param region str region name.
#'
#' @param holdout numeric holdout value (0 for no holdout).
#'
#' @return the path to save the file.
#'
#' @export
mbg_get_nodeenv <- function(node,
                            ig,
                            indic,
                            rd,
                            reg,
                            age,
                            holdout) {
  for (i in node) {
    path_str <- sprintf("DAG_%s_bin%s_%s_%s.RData", i, age, reg, holdout)
    
    img_path <- path_join(
      get_indicator_dir(
        ig, indic
      ),
      "output",
      rd,
      path_str
    )
    
    load(img_path, envir = .GlobalEnv)
  }
    
  return(TRUE)
}
