#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION, Default: gaul_list
#' @param df PARAM_DESCRIPTION
#' @param sample PARAM_DESCRIPTION, Default: 'BOTH'
#' @param subset_shape PARAM_DESCRIPTION, Default: subset_shape
#' @param ind PARAM_DESCRIPTION, Default: indicator
#' @param ind_gp PARAM_DESCRIPTION, Default: indicator_group
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param save.dir PARAM_DESCRIPTION
#' @param year_col PARAM_DESCRIPTION, Default: 'original_year'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[base]{colSums}}
#'  \code{\link[scales]{muted}}
#' @rdname plot_abs_errors
#' @export
plot_abs_errors <- function(gaul_list = gaul_list,
                            df, ## takes output from get_is_oos_draws()
                            sample = "BOTH", ## sample == "IS" or "OOS", or "BOTH"
                            subset_shape = subset_shape,
                            ind = indicator,
                            ind_gp = indicator_group,
                            rd = run_date,
                            save.dir,
                            year_col = "original_year") {

  ## ############################################################
  ## ~~~~~~~~~~~function to plot residual errors ~~~~~~~~~~~~~ ##
  ## ############################################################
  ## takes in a gaul_list over the entire modelling domain (e.g. africa)
  ## takes in subset shape for entire modelling domain
  ## takes in the data.frame that get_is_oos_draws() outputs
  ## save.dir determines output path
  ##
  ## saves plots to output dir and also returns them


  if (ind == "wasting_mod_b") nice.name <- "Wasting"
  if (ind == "stunting_mod_b") nice.name <- "Stunting"
  if (ind == "underweight_mod_b") nice.name <- "Underweight"

  ## setup the dataframe
  subset_shape <- subset_shape[subset_shape$GAUL_CODE %in% gaul_list, ]

  ## rename year col for convenience
  df <- copy(as.data.table(df))
  setnames(df, year_col, "the_year_col")

  ## calculate residual: count/N - pred
  ## calculate residual: count/N - pred
  phat <- base::rowMeans(draws.df[, grep("draw", colnames(df)), with = FALSE], na.rm = TRUE)
  phat[is.nan(phat)] <- NA
  df$phat <- phat
  df$pobs <- df[[ind]] / df[["N"]]
  df$abs_error <- df$pobs - df$phat
  df <- subset(df, !is.na(abs_error))

  if (sample == "IS") to.do <- c(1, 0)
  if (sample == "OOS") to.do <- c(0, 1)
  if (sample == "BOTH") to.do <- c(1, 1)


  full.df <- df
  if (to.do[1] == 1) { ## is

    df <- subset(full.df, fold == 0)

    if (length(df[, GAUL_CODE]) != 0) {
      this_shape.dt <- data.table(fortify(subset_shape))
      redwhiteblue <- c(
        scales::muted("blue"),
        "white",
        scales::muted("red")
      )
      ## plot gg
      gg.is <- ggplot(df, aes(longitude, latitude)) +
        geom_point(aes(
          color = abs_error,
          size = N,
          alpha = weight
        )) +
        coord_fixed() +
        geom_path(
          data = this_shape.dt, aes(x = long, y = lat, group = group),
          color = "black", lwd = .1
        ) +
        scale_color_gradientn(
          colours = redwhiteblue,
          values = c(-1, 0, 1), limits = c(-1, 1),
          na.value = "#000000",
          rescaler = function(x, ...) x,
          oob = identity
        ) +
        guides(color = guide_colorbar(
          title = "Absolute\nerror",
          label = TRUE,
          ticks = FALSE
        )) +
        scale_x_continuous("", breaks = NULL) +
        scale_y_continuous("", breaks = NULL) +
        theme(
          panel.margin = unit(0, "lines"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)
        ) +
        facet_wrap(~the_year_col) +
        ggtitle(paste0(nice.name, " absolute error"))

      ggsave(
        filename = sprintf("%s%s_abs_error_plot_IS.png", save.dir, ind),
        plot = gg.is, width = 12, height = 12, units = "in"
      )
    } else {
      gg.is <- NULL
    }
  }

  if (to.do[2] == 1) { ## oos

    df <- subset(full.df, fold != 0)

    if (length(df[, GAUL_CODE]) != 0) {
      this_shape.dt <- data.table(fortify(subset_shape))
      redwhiteblue <- c(
        scales::muted("blue"),
        "white",
        scales::muted("red")
      )
      ## plot gg
      gg.oos <- ggplot(df, aes(longitude, latitude)) +
        geom_point(aes(
          color = abs_error,
          size = N,
          alpha = weight
        )) +
        coord_fixed() +
        geom_path(
          data = this_shape.dt, aes(x = long, y = lat, group = group),
          color = "black", lwd = .1
        ) +
        scale_color_gradientn(
          colours = redwhiteblue,
          values = c(-1, 0, 1), limits = c(-1, 1),
          na.value = "#000000",
          rescaler = function(x, ...) x,
          oob = identity
        ) +
        guides(color = guide_colorbar(
          title = "Absolute\nerror",
          label = TRUE,
          ticks = FALSE
        )) +
        scale_x_continuous("", breaks = NULL) +
        scale_y_continuous("", breaks = NULL) +
        theme(
          panel.margin = unit(0, "lines"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)
        ) +
        facet_wrap(~the_year_col) +
        ggtitle(paste0(nice.name, " absolute error"))

      ggsave(
        filename = sprintf("%s%s_abs_error_plot_OOS.png", save.dir, ind),
        plot = gg.oos, width = 12, height = 12, units = "in"
      )
    } else {
      gg.oos <- NULL
    }
  }

  return(list(
    is = gg.is,
    oos = gg.oos
  ))
}
