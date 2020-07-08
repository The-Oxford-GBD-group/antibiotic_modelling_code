#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param st PARAM_DESCRIPTION, Default: strata
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param ig PARAM_DESCRIPTION, Default: indicator_group
#' @param baseline_year PARAM_DESCRIPTION, Default: 2000
#' @param measure PARAM_DESCRIPTION, Default: 'prevalence'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname waitforresultstable
#' @export
#' @importFrom stringr str_match
waitforresultstable <- function(st = strata,
                                rd = run_date,
                                indic = indicator,
                                ig = indicator_group,
                                baseline_year = 2000,
                                measure = "prevalence") {
  message("Waiting for results tables to finish...")
  r_left <- length(st)
  u_left <- length(st)
  while (length(r_left) > 0 & length(u_left) > 0) {
    message(paste0("\nCurrent time: ", Sys.time()))
    str_match <- stringr::str_match
    results_dir_r <- paste0("/share/geospatial/mbg/", ig, "/", indic, "/output/", rd, "/table_", baseline_year, "/")
    results_dir_u <- paste0("/share/geospatial/mbg/", ig, "/", indic, "/output/", rd, "/table_", baseline_year, "_unraked/")

    r_regs_done <- list.files(results_dir_r) %>% str_match(., paste0(measure, "_(.*).csv")) %>% .[, 2] %>% .[!is.na(.)]
    u_regs_done <- list.files(results_dir_u) %>% str_match(., paste0(measure, "_(.*).csv")) %>% .[, 2] %>% .[!is.na(.)]

    r_left <- st[!(st %in% r_regs_done)]
    u_left <- st[!(st %in% u_regs_done)]

    message(paste0(c("  Raked tables remaining: ", paste(r_left, collapse = ", "))))
    message(paste0(c("  Unraked tables remaining: ", paste(u_left, collapse = ", "))))

    Sys.sleep(60)
  }
}
