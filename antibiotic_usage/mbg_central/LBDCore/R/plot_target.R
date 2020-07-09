#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param sample PARAM_DESCRIPTION
#' @param subset_shape PARAM_DESCRIPTION
#' @param target PARAM_DESCRIPTION
#' @param target_type PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION, Default: NULL
#' @param shapfile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[Hmisc]{cut2}}
#' @rdname plot_target
#' @export
#' @importFrom Hmisc cut2
plot_target <- function(gaul_list, df, sample, subset_shape, target, target_type, region = NULL, shapfile_version = "current") {
  ## sample == "IS" or "OOS"
  ## region: NULL if a single GAUL, otherwise passes region name for plot
  # based on plot_quilt() by Nick G

  package_lib <- paste0("/home/j/temp/geospatial/packages")
  .libPaths(package_lib)
  cut2 <- Hmisc::cut2 # custom load one function from HMisc (avoid name conflicts)
  loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)

  # get location name - either for single gaul or for region
  if (!is.null(region)) {
    loc_name <- paste0(": ", region)
  } else if (length(gaul_list) == 1) {
    loc_name <- paste0(": ", loc_names[GAUL_CODE == gaul_list, ihme_lc_id])
  } else {
    loc_name <- ""
  }

  message(paste0("\nRunning plot_target for ", substr(loc_name, 3, nchar(loc_name))))

  subset_shape <- subset_shape[subset_shape$GAUL_CODE %in% gaul_list, ]
  df <- as.data.table(df)
  setnames(df, "country", "ihme_lc_id")
  df <- merge(df, loc_names, by = "ihme_lc_id")
  df <- df[GAUL_CODE %in% gaul_list, ]

  # Check for NAs & drop
  if (nrow(df[is.na(get(sample)), ]) > 0) {
    warning(paste0(
      "You have ", nrow(df[is.na(get(sample)), ]), " rows in your data where ", sample, " is NA.",
      " \nThis may reflect edge points from other countries, but check if large number. Dropping..."
    ))
    df <- df[!is.na(get(sample)), ]
  }

  # Create year bins
  df[year >= 1998 & year < 2003, bin_year := 2000]
  df[year >= 2003 & year < 2008, bin_year := 2005]
  df[year >= 2008 & year < 2013, bin_year := 2010]
  df[year >= 2013 & year < 2018, bin_year := 2015]

  # Determine whether the true data hit the target
  df[, outcome := get(indicator) / N]
  df[, hit_target := as.numeric(do.call(get(target_type), list(outcome, target)))]

  # Create an exposure variable (to encapsulate N and weight)
  df[, exposure := N * weight]

  # create graphs
  if (length(df[, GAUL_CODE]) != 0) {
    years <- sort(unique(df$bin_year))

    # by year

    df_summ <- lapply(years, make_year_table) %>%
      rbindlist(.)

    setnames(df_summ, c("5%", "50%", "95%"), c("p5", "p50", "p95"))


    message(">> Building plots...")

    # make calibration plot
    plot_list <- lapply(c(2000, 2005, 2010, 2015), function(this_year) {
      df_plot <- df_summ[year == this_year, ]
      df_plot[, my_fill := "Predicted"]

      if (nrow(df_plot) > 0) {
        p_scatter <- ggplot(df_plot, aes(x = bins)) +
          geom_boxplot(
            stat = "identity",
            aes(lower = p5, ymin = p5, middle = wmean_pred, upper = p95, ymax = p95, fill = my_fill),
            colour = "darkgray"
          ) +
          geom_point(aes(y = wmean_data, shape = "Observed"), colour = "black") +
          scale_x_discrete(drop = F) +
          theme_classic() +
          labs(x = "Prediction bins", y = "P(meets target)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_shape_manual("", values = c("Observed" = 19)) +
          scale_fill_manual("", values = c("Predicted" = "lightgray")) +
          ylim(0, 1)
      } else {
        p_scatter <- ggplot(df_plot) + geom_blank() + theme_classic()
      }
      return(p_scatter)
    })

    # make histograms
    hist_list <- lapply(
      c(2000, 2005, 2010, 2015),
      function(this_year) {
        if (nrow(df_summ[year == this_year, ]) > 0) {
          p_hist <- ggplot(df_summ[year == this_year, ], aes(x = bins)) +
            geom_bar(stat = "identity", aes(y = N_total)) +
            scale_x_discrete(drop = F) +
            theme_classic() +
            labs(y = "N", x = "") +
            theme(
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()
            ) +
            scale_shape_discrete(name = "Observed")
        } else {
          p_hist <- ggplot(df_summ[year == this_year, ]) + geom_blank() + theme_classic()
        }
      }
    )

    # Pull a legend my_legends <- lapply(plot_list, g_legend) # get the first one that works
    my_legends <- my_legends[vapply(my_legends, Negate(is.null), NA)]
    my_legend <- my_legends[[1]]

    plot_list <- lapply(plot_list, function(a_plot) {
      a_plot <- a_plot + theme(legend.position = "none")
    })

    lay <- rbind(
      c(5, 5, 6, 6),
      c(1, 1, 2, 2),
      c(1, 1, 2, 2),
      c(NA, NA, NA, NA),
      c(7, 7, 8, 8),
      c(3, 3, 4, 4),
      c(3, 3, 4, 4)
    )

    # generate a plot for each year
    bin_years <- c(2000, 2005, 2010, 2015)
    year_plots <- lapply(
      1:length(bin_years),
      function(i) {
        year_plot <- arrangeGrob(
          grobs = list(plot_list[[i]], hist_list[[i]]),
          layout_matrix = rbind(2, 1),
          heights = c(0.2, 0.8),
          top = textGrob(
            label = as.character(bin_years[i]),
            gp = gpar(fontsize = 24)
          )
        )
        return(year_plot)
      }
    )

    # generate 4-up of plots (both plot & hist)
    lay <- rbind(
      c(NA, NA),
      c(1, 2),
      c(NA, NA),
      c(3, 4)
    )

    all_plots <- arrangeGrob(grobs = year_plots, layout_matrix = lay, heights = c(0.1, 1, 0.2, 1))

    # add the final legend
    final_graph <- grid.arrange(all_plots, my_legend,
      layout_matrix = rbind(c(1, NA, 2)),
      widths = c(7, 0.2, 0.8),
      top = textGrob(
        label = paste0(
          "Calibration", loc_name,
          " (target ", as.character(target_type), " ",
          as.character(target), ")"
        ),
        gp = gpar(fontsize = 24)
      )
    )
    return(final_graph)
  }
}
