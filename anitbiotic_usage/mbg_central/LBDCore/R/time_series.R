#' @title Create time series plots at the specified admin level, faceting by the number of admin levels in that geography
#'
#' @description This function is called in the subnational_ts_function and is a way to generalize making time
#' series for each admin unit. These functions are all very similar but come in four flavors:
#'
#'
#' 1) time_series:  This plots the model predictions along with the uncertainty, faceted by the admin unit
#' 2) time_series_data: This plots the model predictions and uncertainty but adds the data aggregated to that admin level
#' (using input_aggregated_admin function), and it colors the points by the data source, as well as differentiating between
#' survey size and point and polygon with size and shape aesthetics respectively
#' 3) time_series_multiple: This plots multiple model predictions (either different model runs or multiple indicators, specified by model_runs = T in subnational_ts_plots)
#' on each time trend at the specified admin unit (0/1/2)
#' 4) time_series_multiple_data: This plots multiple model predictions and also includes the aggregated input data to that admin unit.
#' I only recommend using this if comparing different model runs of the same indicator, as plotting the data and multiple indicators makes the plots too busy
#'
#' @author Michael Cork, \email{mcork23@uw.edu}
#'
#' @param model_data admin level data table from the standard aggregation code (typically ad0_df/ad1_df/ad2_df specified to certain geographic area)
#' @param input_data input data collapsed to admin level (typically ad0_data/ad1_data/ad2_data specified to certain geographic area)
#' @param admin specified admin level (0/1/2)
#' @param val_range Range of values for y axis
#' @param title_plot_size Size of plot title
#'
#' @return a ggplot object
#'
#' @examples
#' \dontrun{
#' ## ad0_df_reg is the ad0_df specified to specific region, say for example 'cssa' (Central Sub-Saharan Africa)
#' if (plot_data == F & multiple_runs == F) ad0_time_series <- time_series(ad0_df_reg, admin = 0, val_range = c(0, 1))
#' }
#' @export
time_series <- function(model_data,
                        admin = 0,
                        val_range = c(0, 1),
                        title_plot_size = 30,
                        ind_title = "") {

  # Color palette
  carto_discrete <- rep(c(
    "#7F3C8D", "#11A579", "#F2B701", "#E73F74",
    "#3969AC", "#80BA5A", "#E68310", "#008695",
    "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"
  ), 3)
  gg_admin <-
    ggplot(data = model_data, aes(x = year, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.3) +
    geom_line() +
    theme_bw(base_size = 16) +
    coord_cartesian(ylim = val_range) +
    facet_wrap(~plot_name) +
    theme(
      strip.background = element_blank(),
      plot.caption = element_text(hjust = 0.5),
      plot.title = element_text(size = title_plot_size, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
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
