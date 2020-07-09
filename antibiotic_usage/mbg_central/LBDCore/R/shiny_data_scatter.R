#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param year_var PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname shiny_data_scatter
#' @export
shiny_data_scatter <- function(df, run_date, indicator, indicator_group, year_var) {

  # Make dirs
  load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, ".RData"))
  output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  plot_dir <- paste0(output_dir, "/plots")
  dir.create(plot_dir, showWarnings = FALSE)

  # Make data coverage scatter
  df <- as.data.table(df)
  df[, clusters := 1]
  total_clusters <- df[, list(clusters = sum(clusters)), by = c("original_year", "country", "source")]
  total_cluster_num <- total_clusters[, list(clusters = sum(clusters))]
  png(paste0(plot_dir, "/data_scatter.png"), width = 1200, height = 800)
  clusters.gg <- ggplot() +
    geom_point(data = total_clusters, aes(x = original_year, y = country, size = clusters, shape = factor(source), color = factor(source))) +
    guides(color = FALSE) +
    scale_size(guide = guide_legend(title = "Clusters/polygons"), range = c(1, 10)) +
    ggtitle(paste0("Clusters/polygons by country/year, total points = ", total_cluster_num[1, clusters])) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  clusters.gg
  dev.off()
}
