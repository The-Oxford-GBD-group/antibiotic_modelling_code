#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param return_graph PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname graph_run_summary
#' @export
graph_run_summary <- function(run_date,
                              indicator,
                              indicator_group,
                              return_graph = F) {

  # Function to graph run_summary files
  # Jon Mosser (jmosser@uw.edu)
  # Requires creation of a run_summary .csv file in your run_date directory

  if (Sys.info()["sysname"] == "Linux") {
    j_root <- "/home/j/"
  }


  dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/")

  file <- list.files(dir, pattern = "run_summary.*.csv")

  # Catch if file does not exist
  if (length(file) == 0) {
    message("No run summary file found to graph... exiting function.")
    return(NULL)
  }

  # Otherwise continue and create file name
  file <- paste0(dir, file)

  df <- read.csv(file, stringsAsFactors = F) %>% as.data.table()


  df$time <- sapply(df$time, grab_time_hours)
  df$step <- factor(df$step, levels = c(
    "Stacking - GAM", "Stacking - GBM", "Stacking - lasso", "Stacking - ridge", "Stacking - enet",
    "MBG - fit model", "MBG - predict model", "Cross-validation",
    "Stacking - all", "MBG - all", "Entire script"
  ))

  summary_steps <- c("Stacking - all", "MBG - all", "Entire script")

  df_graph <- subset(df, !(step %in% summary_steps))
  df_graph[, holdout := as.character(holdout != 0)]

  g_plot <- ggplot(data = df_graph, aes(x = step, y = time, color = region)) +
    stat_summary(fun.y = mean, geom = "line", aes(group = region, color = region)) +
    geom_point(aes(color = region, shape = holdout)) +
    scale_color_brewer(palette = "Set1") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Step",
      y = "Time (hours)",
      title = paste0("Run Date: ", run_date),
      color = "Region",
      shape = "Holdout?"
    )

  png(
    filename = paste0(dir, "run_summary_", indicator, "_", run_date, ".png"),
    type = "cairo",
    units = "in",
    width = 8,
    height = 4.5,
    pointsize = 12,
    res = 300
  )

  print(g_plot)

  dev.off()

  if (return_graph == T) return(g_plot)
}
