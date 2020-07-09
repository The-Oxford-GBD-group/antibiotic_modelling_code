#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param ig PARAM_DESCRIPTION, Default: indicator_group
#' @param ages PARAM_DESCRIPTION
#' @param regions PARAM_DESCRIPTION
#' @param holdouts PARAM_DESCRIPTION
#' @param raked PARAM_DESCRIPTION
#' @param dir_to_search PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname waitforaggregation
#' @export
waitforaggregation <- function(rd = run_date,
                               indic = indicator,
                               ig = indicator_group,
                               ages,
                               regions,
                               holdouts,
                               raked,
                               dir_to_search = NULL) {

  # waitformodelstofinish() analog for aggregation
  # Jon Mosser / jmosser@uw.edu

  if (is.null(dir_to_search)) {
    dir_to_search <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/")
  }

  lv <- expand.grid(regions, holdouts, ages, raked) %>% as.data.table()
  names(lv) <- c("reg", "holdout", "age", "raked")
  lv[, file := paste0(dir_to_search, "fin_agg_", reg, "_", holdout, "_", age, "_", raked)]
  n_left <- nrow(lv)

  message("Waiting for aggregation to finish...")

  while (n_left > 0) {
    message(paste0(
      "\n=====================================",
      "\nCurrent time: ", Sys.time()
    ))

    lv[, file_exists := file.exists(file)]
    n_left <- nrow(lv[file_exists == F])
    lv_left <- lv[file_exists == F]

    if (n_left > 0) {
      message(paste0("Aggregation jobs remaining: ", n_left))
      for (i in 1:nrow(lv_left)) {
        message(paste0(
          "  ",
          "Region: ", lv_left[i, reg], " | ",
          "Holdout: ", lv_left[i, holdout], " | ",
          "Age: ", lv_left[i, age], " | ",
          "Raked: ", lv_left[i, raked]
        ))
      }
    } else {
      break
    }

    Sys.sleep(60)
  }
}
