#' @title Plot stacker betas
#' @description  Create a plot of stacker betas
#'
#' @param model_results table from [whatever]_model_results_table.csv
#' @param stackers vector containing names of stackers
#' @return ggplot object plotting stackers by region
#' @examples
#' \dontrun{
#' # Plot stacker betas
#' gg_betas <- plot_stacker_betas(
#'   model_results = model_results,
#'   stackers = stackers
#' )
#' }
#' @export
plot_stacker_betas <- function(model_results, stackers, xaxis = "stacker") {
  stacker_beta_table <- subset(model_results, parameter %in% stackers)
  dodge <- position_dodge(width = 0.4)
  if (xaxis == "stacker") {
    gg <- ggplot(data = stacker_beta_table, aes(x = parameter, y = q0.5, color = region)) +
      geom_point(position = dodge) +
      geom_pointrange(aes(ymin = q0.025, ymax = q0.975), position = dodge) +
      labs(x = "Stacker", y = "Beta", color = "Region") +
      scale_color_manual(values = get_color_scheme("carto_discrete")) +
      theme_classic()
  } else if (xaxis == "region") {
    gg <- ggplot(data = stacker_beta_table, aes(x = region, y = q0.5, color = parameter)) +
      geom_point(position = dodge) +
      geom_pointrange(aes(ymin = q0.025, ymax = q0.975), position = dodge) +
      labs(x = "Region", y = "Beta", color = "Stacker") +
      scale_color_manual(values = get_color_scheme("carto_discrete")) +
      theme_classic()
  }
  return(gg)
}
