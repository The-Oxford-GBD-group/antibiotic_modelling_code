#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ind.gp PARAM_DESCRIPTION, Default: indicator_group
#' @param ind PARAM_DESCRIPTION, Default: indicator
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param output.dir PARAM_DESCRIPTION, Default: si.fig.dir
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @param title PARAM_DESCRIPTION, Default: 'Comparison to GBD 2016 in \\n'. Region gets pasted on to this
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot.rfs
#' @export
plot.rfs <- function(ind.gp = indicator_group,
                     ind = indicator,
                     rd = run_date,
                     output.dir = si.fig.dir,
                     shapefile_version = "current",
                     title = "Comparison to GBD 2016 in\n") {

  ## ####################################
  ## a function to plot raking factors ##
  ## ####################################
  ## this function plots a scatterplot of GBD estimates vs MBG estimates

  regions <- get_output_regions(in_dir = paste0(
    "/share/geospatial/mbg/",
    ind.gp, "/",
    ind, "/output/",
    rd
  ))

  for (rr in regions) {

    ## convert rrs to full names
    if (rr == "essa") rr_name <- "Eastern Sub-Saharan Africa"
    if (rr == "wssa") rr_name <- "Western Sub-Saharan Africa"
    if (rr == "name") rr_name <- "North Africa"
    if (rr == "sssa") rr_name <- "Southern Sub-Saharan Africa"
    if (rr == "cssa") rr_name <- "Central Sub-Saharan Africa"

    in_dir <- paste0("/share/geospatial/mbg/", ind.gp, "/", ind, "/output/", rd)
    default_rf_path <- paste0(in_dir, "/", ind, "_rf.csv")
    all_rfs <- fread(default_rf_path)
    gaul_list <- get_adm0_codes(rr, shapefile_version = shapefile_version)
    rfs <- all_rfs[name %in% gaul_list, ]
    loc_names <- setDT(get_location_code_mapping(shapefile_version = shapefile_version))
    setnames(rfs, "name", "GAUL_CODE")
    rfs <- merge(rfs, loc_names, by = "GAUL_CODE")
    rfs[, Year := as.factor(year)]
    max_val <- max(max(rfs[, .(rake_to_mean, geo_mean)], na.rm = T), na.rm = T)

    ## plot w/o country labels
    gg_rfs <- ggplot(data = rfs, aes(x = rake_to_mean, y = geo_mean)) +
      geom_point(aes(color = Year)) +
      ylab("MBG Mean") +
      xlab("GBD Mean") +
      theme_bw() +
      xlim(0, max_val) +
      ylim(0, max_val) +
      geom_abline(slope = 1) +
      ggtitle(paste0(title, rr_name))

    assign(sprintf("%s_rf", rr), gg_rfs)

    ## plot w/ country labels
    gg_rfs <- ggplot(data = rfs, aes(x = rake_to_mean, y = geo_mean)) +
      geom_point(aes(color = Year)) +
      geom_text_repel(aes(label = ihme_lc_id),
        segment.color = "grey80"
      ) +
      ylab("MBG Mean") +
      xlab("GBD Mean") +
      theme_bw() +
      xlim(0, max_val) +
      ylim(0, max_val) +
      geom_abline(slope = 1) +
      ggtitle(paste0(title, rr_name))

    assign(sprintf("%s_rf_labs", rr), gg_rfs)
  }

  ## stick them all together
  margin <- theme(plot.margin = unit(rep(.5, 4), "cm"))
  all.rfs <- grid.arrange(cssa_rf + margin,
    essa_rf + margin,
    name_rf + margin,
    sssa_rf + margin,
    wssa_rf + margin,
    ncol = 2
  )
  ggsave(
    filename = sprintf(
      "%s%s_all_rfs.png",
      output.dir, ind
    ),
    all.rfs, width = 12, height = 16
  )

  ## stick them all together
  margin <- theme(plot.margin = unit(rep(.5, 4), "cm"))
  all.rfs <- grid.arrange(cssa_rf_labs + margin,
    essa_rf_labs + margin,
    name_rf_labs + margin,
    sssa_rf_labs + margin,
    wssa_rf_labs + margin,
    ncol = 2
  )
  ggsave(
    filename = sprintf(
      "%s%s_all_rfs_labs.png",
      output.dir, ind
    ),
    all.rfs, width = 12, height = 16
  )
}
