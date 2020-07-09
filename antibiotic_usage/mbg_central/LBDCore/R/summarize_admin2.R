#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param nperiod PARAM_DESCRIPTION
#' @param master_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[seegSDM]{character(0)}}
#' @rdname summarize_admin2
#' @export
summarize_admin2 <- function(gaul, indicator, indicator_group, run_date, nperiod, master_list) {
  message(paste0("Working on ", gaul, "..."))
  # survey_year <- 2015
  # nperiod <- total_periods
  # this_period <- survey_year - 2000 + 1

  ## Load draws and simple_raster by country. Create pixel_index so we don't mess up sorting.
  cell_pred.dt <- as.data.table(master_list[[paste0("list_", gaul, ".draws_", gaul)]])
  cell_pred.dt[cell_pred.dt < 0] <- 0
  names <- names(cell_pred.dt)
  cols <- names(cell_pred.dt)
  period_index <- c()
  for (i in 1:nperiod) {
    period_index <- c(period_index, rep(paste0("period_", i), length(cell_pred.dt$V1) / nperiod))
  }
  pixel_id <- c(rep(
    1:(length(cell_pred.dt$V1) / nperiod),
    nperiod
  ))
  cell_pred.dt <- cbind(cell_pred.dt, period_index, pixel_id)

  ## Summarize and subset
  # cell_pred.dt <- cell_pred.dt[period_index == paste0('period_', this_period), ]

  ## Get pops
  country_pops <- master_list[[paste0("list_", gaul, ".pops_", gaul)]]
  country_pops <- crop(country_pops, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
  country_pops <- setExtent(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
  country_pops <- mask(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])

  ## Get admin2 codes
  country_admin2 <- master_list[[paste0("list_", gaul, ".admin2_", gaul)]]
  country_admin2 <- crop(country_admin2, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
  country_admin2 <- setExtent(country_admin2, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
  country_admin2 <- mask(country_admin2, master_list[[paste0("list_", gaul, ".simple_", gaul)]])

  ## Get ids for all cells
  cell_idx <- notMissingIdx(master_list[[paste0("list_", gaul, ".simple_", gaul)]])

  ## Make full datatable of draws with admin2 and pop info
  geo.dt <- cell_pred.dt
  # admin2_codes <- rep(extract(country_admin2, cell_idx), nperiod)
  geo.dt <- rbindlist(lapply(1:nperiod, pull_period_pops))
  geo.dt <- geo.dt[is.na(pops), pops := 0] # weighted.mean doesn't like NA weights

  ## Make national and admin2 population-weighted means for each draw
  natl_mean_draws <- geo.dt[, lapply(.SD, weighted.mean, w = pops, na.rm = TRUE), by = c("period_index"), .SDcols = grep("^V", names(geo.dt)) ]
  admin2_mean_draws <- geo.dt[, lapply(.SD, weighted.mean, w = pops, na.rm = TRUE), by = c("period_index", "admin2"), .SDcols = grep("^V", names(geo.dt)) ]
  ## Make national and admin2 summaries across draws
  natl_mean_draws <- natl_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = grep("^V", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, mean := apply(.SD, 1, mean), .SDcols = grep("^V", names(natl_mean_draws))]
  natl_mean_draws <- natl_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = grep("^V", names(natl_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm = T), .SDcols = grep("^V", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, mean := apply(.SD, 1, mean), .SDcols = grep("^V", names(admin2_mean_draws))]
  admin2_mean_draws <- admin2_mean_draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm = T), .SDcols = grep("^V", names(admin2_mean_draws))]
  ## Make plots
  rfs <- fread(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_rf.csv"))
  natl_mean_draws <- natl_mean_draws[, year := as.numeric(gsub("period_", "", period_index)) + 2000 - 1]
  natl_mean_draws <- natl_mean_draws[, c("mean", "upper", "lower", "year"), with = FALSE]
  admin2_mean_draws <- admin2_mean_draws[, year := as.numeric(gsub("period_", "", period_index)) + 2000 - 1]
  admin2_mean_draws <- admin2_mean_draws[, c("mean", "upper", "lower", "year", "admin2"), with = FALSE]
  high_admin2 <- admin2_mean_draws[order(-mean)]
  high_admin2 <- high_admin2[year == 2015, .SD[1:1]]
  high_admin2 <- high_admin2[, admin2]
  low_admin2 <- admin2_mean_draws[order(mean)]
  low_admin2 <- low_admin2[year == 2015, .SD[1:1]]
  low_admin2 <- low_admin2[, admin2]
  admin2_mean_draws <- admin2_mean_draws[admin2 %in% high_admin2, Category := "MBG highest admin2"]
  admin2_mean_draws <- admin2_mean_draws[admin2 %in% low_admin2, Category := "MBG lowest admin2"]
  admin2_mean_draws <- admin2_mean_draws[!is.na(Category), ]
  natl_mean_draws <- natl_mean_draws[, Category := "MBG admin0"]
  setnames(rfs, "rake_to_mean", "mean")
  rfs <- rfs[name == gaul, c("mean", "year"), with = FALSE]
  rfs <- rfs[, Category := "GBD admin0"]
  all_data <- rbind(natl_mean_draws, admin2_mean_draws, rfs, fill = TRUE)

  gaul_gg <- ggplot() +
    ## Admin2s
    geom_line(
      data = all_data,
      aes(
        x = year,
        y = mean,
        group = Category,
        color = Category,
        size = Category
      )
    ) +
    geom_ribbon(
      data = all_data,
      aes(
        x = year,
        ymin = lower,
        ymax = upper,
        group = Category,
        fill = Category
      ),
      alpha = 0.2
    ) +
    scale_colour_manual(
      values = c("black", "#2ca25f", "#2b8cbe", "#e34a33")
    ) +
    scale_fill_manual(
      values = c("black", "#2ca25f", "#2b8cbe", "#e34a33")
    ) +
    scale_size_manual(
      values = c(2, 2, 1, 1)
    ) +
    theme_minimal() +
    ylab("Mean\noutcome") +
    xlab("Year")

  return(gaul_gg)
}
