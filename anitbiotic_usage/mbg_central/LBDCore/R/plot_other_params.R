#' @title Plot other parameters
#' @description Create a plot of other parameters from INLA fit
#'
#' @param model_results table from [whatever]_model_results_table.csv
#' @param other_params names of other parameters (not stackers)
#' @return ggplot object plotting parametersby region
#' @examples
#' \dontrun{
#' # Plot stacker betas
#' # Plot other parameters
#' gg_other_params <- plot_other_params(
#'   model_results = model_results,
#'   other_params = other_params
#' )
#' }
#' @export
plot_other_params <- function(model_results, other_params) {
  other_param_table <- subset(model_results, parameter %in% other_params)
  dodge <- position_dodge(width = 0.4)
  ggplot(data = other_param_table, aes(x = region, y = q0.5, color = region)) +
    geom_point(position = dodge) +
    geom_pointrange(aes(ymin = q0.025, ymax = q0.975), position = dodge) +
    labs(x = "Region", y = "Value", color = "Region") +
    facet_wrap(~parameter, scales = "free_y") +
    scale_color_manual(values = get_color_scheme("carto_discrete")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
