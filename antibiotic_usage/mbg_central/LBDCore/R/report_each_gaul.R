## Function to run each country in a region
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul PARAM_DESCRIPTION
#' @param quilt_plot_list PARAM_DESCRIPTION
#' @param target_plot_list PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname report_each_gaul
#' @export
report_each_gaul <- function(gaul, quilt_plot_list, target_plot_list, shapefile_version = "current") {
  message(paste0("REPORT FOR ", gaul, ":"))

  ##################################################################################
  ## Face validity checks
  ##################################################################################
  ##  - Raking factors
  ##  - DHS admin1 comparison
  ##  - Semivariograms
  ##  - Time series, GBD vs. MBG
  ##################################################################################
  ## Plot 2000, 2005, 2010, 2015 predictions, raked and unraked
  pred_maps <- extract_raked_and_unraked(gaul,
    results,
    results_raked,
    master_list,
    admin0 = admin0
  )

  ## Plot raking factors (all + by country)
  country_rf_plot <- val_raking_factors(
    gaul_list = gaul,
    rfs = all_rfs
  )

  ## DHS admin1 validation (all + by country)

  ## (???) Comparison to some threshold (full Bayesian draw-level)

  ## Semivariograms of raw data and predictions at raw data locations (by country)
  vario_and_violin_plots <- plot_varios_and_violins(
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    this_gaul = gaul
  )

  ## GBD and MBG admin0 estimates by year with credible intervals
  country_time_series_plots <- summarize_admin2(
    gaul = gaul,
    indicator = indicator,
    indicator_group = indicator_group,
    run_date = run_date,
    nperiod = total_periods,
    master_list = master_list
  )

  if (indicator == "edu_mean") {

    ## Load premade .csv of DHS admin1 aggregates for edu_mean and an SPDF with all the relevant location_code/shapefile entries.
    load("/share/geospatial/mbg/validation_polygons/dhs_admin1.RData")
    dhs_admin1_data <- fread("/share/geospatial/mbg/validation_data/edu_mean_dhs_admin1.csv")

    ## Make plot for each NID in this country
    loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)
    this_iso3 <- loc_names[GAUL_CODE %in% gaul, ihme_lc_id]
    dhs_admin1_data <- dhs_admin1_data[iso3 == this_iso3 & year >= 2000, ]
    iso3_nid_list <- unique(dhs_admin1_data[, nid])

    ## Plot comparison
    unraked_dhs_plots <- lapply(iso3_nid_list, plot_each_nid_unraked)
    raked_dhs_plots <- lapply(iso3_nid_list, plot_each_nid_raked)
  }

  ##################################################################################
  ## Statistical validity
  ##################################################################################
  ##  - Covariate relative importance
  ##  - In-sample and out-of-sample error (quiltplots, tables)
  ##################################################################################
  message("VALIDATION STEP 2: Running statistical validity functions...")

  ## Covariate importance

  ## Quiltplots (bias, coverage, RMSE), also summarize into tabulated tables

  ##################################################################################
  ## Make reports
  ##################################################################################
  ##  - All gauls
  ##  - By country
  ##################################################################################
  message("Making final reports for all countries and each country individually...")

  # plot(vario_and_violin_plots[[2]], main="PACF")
  # grid.echo()
  # pacf <- grid.grab()
  # pacf$vp <- viewport(layout.pos.row = 11:14, layout.pos.col = 9:16)

  # grab your legends using the predefined functions, then state their grid location
  map.legend <- gLegend(pred_maps[["raked"]])
  map.legend$vp <- viewport(layout.pos.row = 1:10, layout.pos.col = 17:18)

  # Initialize plot with master title
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(28, 18)))
  # Plot all pred maps
  grid.text("Unraked", vp = vplayout(1, 1:16), gp = gpar(fontsize = 20, fontface = "bold"))
  # print(pred_maps[['unraked']][[1]] + theme(legend.position="none"), vp = vplayout(2:5, 1:4))
  # print(pred_maps[['unraked']][[2]] + theme(legend.position="none"), vp = vplayout(2:5, 5:8))
  # print(pred_maps[['unraked']][[3]] + theme(legend.position="none"), vp = vplayout(2:5, 9:12))
  # print(pred_maps[['unraked']][[4]] + theme(legend.position="none"), vp = vplayout(2:5, 13:16))
  print(pred_maps[["unraked"]] + theme(legend.position = "none"), vp = vplayout(2:7, 1:16))
  grid.text("Raked", vp = vplayout(8, 1:16), gp = gpar(fontsize = 20, fontface = "bold"))
  # print(pred_maps[['raked']][[1]] + theme(legend.position="none"), vp = vplayout(7:10, 1:4))
  # print(pred_maps[['raked']][[2]] + theme(legend.position="none"), vp = vplayout(7:10, 5:8))
  # print(pred_maps[['raked']][[3]] + theme(legend.position="none"), vp = vplayout(7:10, 9:12))
  # print(pred_maps[['raked']][[4]] + theme(legend.position="none"), vp = vplayout(7:10, 13:16))
  print(pred_maps[["raked"]] + theme(legend.position = "none"), vp = vplayout(9:14, 1:16))
  grid.draw(map.legend)
  # Plot vario
  # print(vario_and_violin_plots[[1]] + theme(legend.position="none"), vp = vplayout(11:14, 1:16))
  # grid.draw(pacf)
  # Plot raking factors and violins
  print(country_rf_plot + theme(legend.position = "none"), vp = vplayout(15:21, 1:8))
  print(vario_and_violin_plots[[3]] + theme(legend.position = "none"), vp = vplayout(15:21, 9:16))
  # Admin plots
  print(country_time_series_plots, vp = vplayout(22:28, 1:16))

  ## Make new page with threshold plot and covariate importance
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(28, 18)))
  this_target_plot <- target_plot_list[[as.character(gaul)]]
  this_target_plot$vp <- viewport(layout.pos.row = 1:16, layout.pos.col = 1:18)
  grid.draw(this_target_plot)

  ## Make new page with quilt plots, if they exist
  if (!is.null(quilt_plot_list[[as.character(gaul)]])) {
    quilt.legend <- gLegend(quilt_plot_list[[as.character(gaul)]])
    quilt.legend$vp <- viewport(layout.pos.row = 1:15, layout.pos.col = 17:18)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(28, 18)))
    # Plot all quilt maps for years that exist
    grid.text("In-Sample", vp = vplayout(1, 1:16), gp = gpar(fontsize = 20, fontface = "bold"))
    print(quilt_plot_list[[as.character(gaul)]] + theme(legend.position = "none"), vp = vplayout(2:16, 1:15))
    grid.draw(quilt.legend)
  }

  ## Plot DHS admin1 comparison for education
  if (indicator == "edu_mean") {
    if (length(unraked_dhs_plots) != 0) {
      for (i in 1:length(unraked_dhs_plots)) {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(28, 18)))
        print(unraked_dhs_plots[[i]], vp = vplayout(1:13, 1:16))
        print(raked_dhs_plots[[i]], vp = vplayout(15:27, 1:16))
      }
    }
  }

  return(NULL)
}
