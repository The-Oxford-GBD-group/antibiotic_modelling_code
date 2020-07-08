#' @title Add model run info
#' @description This function is used to add a "run" column specifying a model run or model indicators to be used in multiple runs, when
#' plotting model predictions of multiple runs. The input is a list of model prediction of a specific admin unit, and the output is
#' a data table that binds the predictions together with a column titled "run" that specified what indicator/model run the model
#' prediction is from, dependent on the run label.
#'
#' @author Michael Cork, \email{mcork23@uw.edu}
#'
#' @param prediction_list List where each component is a model prediction at a specified admin level
#' @param run_label This labels each model run/different indicator so that we can eventually facet by the run
#'                  column when creating a ggplot2 object
#'
#'
#' @return
#' This function returns a single data table that binds all of prediction list components together, adding the appropriate "run" column
#'
#' @examples
#' \dontrun{
#' # Example of code used for adding multiple indicators to a subnational_ts_plot
#' run_dates <- c("2018_05_30_23_01_59", "2018_03_23_16_36_23")
#' run_label <- c("STI symp", "Condom use")
#' indicators <- c("condom_last_time", "sti_symptoms")
#' 
#' share_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicators, "/output/", run_dates, "/")
#' in_dir <- paste0(share_dir, "pred_derivatives/admin_summaries/")
#' out_dir <- paste0(share_dir, "admin_plots/")
#' 
#' in_file_ad0 <- paste0(in_dir, indicators, "_admin_0_unraked_summary.csv")
#' in_file_ad1 <- paste0(in_dir, indicators, "_admin_1_unraked_summary.csv")
#' in_file_ad2 <- paste0(in_dir, indicators, "_admin_2_unraked_summary.csv")
#' 
#' ad0_df <- lapply(in_file_ad0, fread) %>% add_run_label(run_label)
#' ad1_df <- lapply(in_file_ad1, fread) %>% add_run_label(run_label)
#' ad2_df <- lapply(in_file_ad2, fread) %>% add_run_label(run_label)
#' # These inputs are now ready to be passed into subnational_ts_plots with multiple_runs = T
#' }
#' @export
add_run_label <- function(prediction_list, run_label = NULL) {
  for (i in 1:length(prediction_list)) {
    prediction_list[[i]][["run"]] <- run_label[i]
  }
  x <- rbindlist(prediction_list)
  x[, run := factor(run, run_label)]
  return(x)
}
