#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model_data PARAM_DESCRIPTION
#' @param input_data PARAM_DESCRIPTION
#' @param admin PARAM_DESCRIPTION, Default: 0
#' @param val_range PARAM_DESCRIPTION, Default: c(0, 1)
#' @param title_plot_size PARAM_DESCRIPTION, Default: 30
#' @param ind_title PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname time_series_data
#' @export
time_series_data <- function(model_data,
                             input_data,
                             admin = 0,
                             val_range = c(0, 1),
                             title_plot_size = 30,
                             ind_title = "") {

  #### Plot time series trends with aggregated input_data

  # Color palette
  carto_discrete <- rep(c(
    "#7F3C8D", "#11A579", "#F2B701", "#E73F74",
    "#3969AC", "#80BA5A", "#E68310", "#008695",
    "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"
  ), 3)
  gg_admin <-
    ggplot() +
    geom_ribbon(data = model_data, aes(x = year, ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(data = model_data, aes(x = year, y = mean)) +
    theme_bw(base_size = 16) +
    geom_point(
      data = input_data,
      aes(x = year, y = outcome, size = N, shape = as.factor(point), fill = as.factor(source)), alpha = 0.7
    ) +
    scale_fill_manual("Survey", values = carto_discrete, drop = F) +
    scale_shape_manual("Type",
      breaks = c("0", "1", "2", "3"), values = c(22, 21, 12, 10),
      label = c("polygon", "point", "Subnationally \n Representative", "Subnationally \n Representative"), drop = F
    ) +
    facet_wrap(~plot_name) +
    coord_cartesian(ylim = val_range) +
    scale_size(range = c(1, 7)) +
    theme(
      strip.background = element_blank(),
      plot.caption = element_text(hjust = 0.5),
      plot.title = element_text(size = title_plot_size, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 7),
      legend.justification = "top"
    ) +
    guides(
      fill = guide_legend(order = 1, override.aes = list(size = 5, shape = 21)),
      shape = guide_legend(order = 2, override.aes = list(size = 5))
    ) +
    labs(
      y = ind_title,
      x = "Year"
    ) + {
      if (admin == 0) labs(title = paste0(ind_title, " by Country"))
    } + {
      if (admin == 1) {
        labs(
          title = paste0(ind_title, " by First-level Administrative Unit"),
          caption = paste0(
            "Time series depict first-level administrative units except for NATIONAL,\n",
            "which shows the time series for the entire country"
          )
        )
      }
    } + {
      if (admin == 2) {
        labs(
          title = paste0(ind_title, " by Second-level Administrative Unit"),
          caption = paste0(
            "Plots shown are for second-level administrative units except for ADMIN 1,\n",
            "which shows the time series for the entire first-level administrative unit"
          )
        )
      }
    }
  return(gg_admin)
}
