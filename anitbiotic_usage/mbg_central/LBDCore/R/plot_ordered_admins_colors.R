#' @title "Traffic light" plots and maps to help visualize categories of administrative estimates
#'
#' @description This is currently written to work for three broad categories (in the original case,
#' vaccine coverage 0-60%, 60-90%, and 90-100%) which are colored red, yellow, and blue.
#' Borderline estimates (i.e. with CIs that overlap 60% or 90% in the original example) are
#' colored orange and green.  If CIs extend into all 3 categories, then the estimate
#' is colored gray.
#'
#' Currently, this is relatively inflexible - ideally would be extended to include
#' other category/color schemes and work with admin1s.  Feel free to submit a PR!
#'
#' @author Jon Mosser, \email{jmosser@uw.edu}
#'
#' @param df data.frame or data.table from the aggregation code.  Needs to have
#'           columns for \code{mean}, \code{upper}, \code{lower}, \code{ADM2_CODE}, \code{ADM0_CODE}, \code{year}, and also a
#'           also a \code{pop} column which can be obtained as in the example (from merging on the
#'           pops from the admin draws object)
#' @param ad0_name the \code{ADM0_NAME} that you're interested in plotting. Can loop over this
#'                 in a wrapper function to be able to produce plots for many countries
#'                 sequentially
#' @param plot_year which year in df would you like to plot?
#' @param indicator title for your indicator, e.g. "DPT3 Coverage"
#' @param pop_title title for your population, e.g. "Population: children < 5 years"
#' @param cutoffs vector of cutoffs for your plot. For instance \code{c(0, 0.6, 0.9, 1)}.
#'                Must be 4 items (3 bins) for now
#' @param error_bars would you like error bars? Either "yaxis" or "none"
#' @param ad2_shp admin2 shape for plotting
#' @param ad0_shp admin0 shape for national borders
#' @param verbose more updates from the function (Boolean)
#' @param shapefile_version string indicating version of shapefile to pull
#'
#' @return a grob object for the full plot. Note: need to use \code{grid.draw()} to plot grobs!
#'
#' @examples
#' \dontrun{
#' # Set up directories and files
#' # Set `run_date`, `indicator`, `indicator_group`, `out_dir` as you wish
#' 
#' share_dir <- paste0(
#'   "/share/geospatial/mbg/", indicator_group, "/",
#'   indicator, "/output/", run_date, "/"
#' )
#' in_dir <- paste0(share_dir, "pred_derivatives/admin_summaries/")
#' in_file_ad2 <- paste0(in_dir, indicator, "_admin_2_raked_summary.csv")
#' ad2_df <- fread(in_file_ad2)
#' 
#' # Drop Ma'tan al-Sarra if present
#' ad2_df <- subset(ad2_df, ADM0_CODE != 40762)
#' 
#' # Merge on populations
#' load(paste0(share_dir, indicator, "_raked_admin_draws_eb_bin0_0.RData"))
#' pops <- subset(admin_2, year %in% unique(ad2_df$year), select = c("ADM2_CODE", "pop", "year"))
#' ad2_df <- merge(ad2_df, pops, by = c("ADM2_CODE", "year"), all.x = T, all.y = F)
#' rm(admin_2)
#' 
#' # Use GAUL shapefiles as defaults
#' ad0_shp <- readRDS("/share/geospatial/rds_shapefiles/background_map_africa/background_map_africa.rds")
#' ad2_shp <- readOGR(get_admin_shapefile(admin_level = 2))
#' 
#' a_grob <- plot_ordered_admins_colors(
#'   df = ad2_df,
#'   ad0_name = "Senegal",
#'   plot_year = 2016,
#'   indicator_title = "DPT3 Coverage",
#'   pop_title = "Population: Children < 5 years",
#'   cutoffs = c(0, 0.6, 0.9, 1),
#'   error_bars = "yaxis",
#'   ad2_shp = ad2_shp,
#'   ad0_shp = ad0_shp,
#'   verbose = T
#' )
#' 
#' grid.draw(a_grob)
#' }
#' 
#' @export
plot_ordered_admins_colors <- function(df,
                                       ad0_name,
                                       plot_year,
                                       indicator_title,
                                       pop_title,
                                       cutoffs,
                                       error_bars = "yaxis",
                                       ad2_shp = NULL,
                                       ad0_shp = NULL,
                                       verbose = F,
                                       shapefile_version = "current") {

  ################################################################################
  # 0. SETUP #####################################################################
  ################################################################################

  if (verbose) message(paste0("Working on ", ad0_name, "..."))


  # Check inputs
  if (length(cutoffs) != 4) stop("Currently only three-bin estimates supported - length(cutoffs) should be 4")

  # Use GAUL shapefiles as defaults
  if (is.null(ad0_shp)) ad0_shp <- rgdal::readOGR(get_admin_shapefile(admin_level = 0, version = shapefile_version))
  if (is.null(ad2_shp)) ad2_shp <- rgdal::readOGR(get_admin_shapefile(admin_level = 2, version = shapefile_version))

  # Subset df to the country of interest & year and sort
  df <- copy(as.data.table(df))
  df <- subset(df, year == plot_year)
  if (ad0_name != "all") {
    df <- copy(subset(df, ADM0_NAME == ad0_name))
    country_title <- unique(df$ADM0_NAME)
  } else {
    country_title <- "Africa (all countries)"
  }

  xaxis_title <- "Second-level administrative unit (ordered by mean MBG estimate)"
  sort_table <- subset(df[order(mean)], select = c("mean", "ADM2_CODE"))
  sort_table[, plot_idx := .I]
  plot_df <- merge(df, subset(sort_table, select = c("ADM2_CODE", "plot_idx")), by = "ADM2_CODE")

  # Assign color categories
  c_2 <- cutoffs[2]
  c_3 <- cutoffs[3]
  clab_2 <- paste0(c_2 * 100, "%")
  clab_3 <- paste0(c_3 * 100, "%")

  cat_levels <- c(
    paste0("Significantly < ", clab_2),
    paste0("Near ", clab_2, " with overlapping UIs"),
    paste0("Significantly above ", clab_2, " and below ", clab_3),
    paste0("Near ", clab_3, " with overlapping UIs"),
    paste0("Significantly > ", clab_3),
    "UIs overlap all categories"
  )

  plot_df[upper < c_2, category := cat_levels[1]]
  plot_df[lower < c_2 & upper >= c_2 & upper <= c_3, category := cat_levels[2]]
  plot_df[lower > c_2 & upper < c_3, category := cat_levels[3]]
  plot_df[lower >= c_2 & lower <= c_3 & upper > c_3, category := cat_levels[4]]
  plot_df[lower > c_3, category := cat_levels[5]]
  plot_df[is.na(category), category := cat_levels[6]]

  plot_df$category <- factor(plot_df$category, levels = cat_levels)

  # Set up a color scheme
  color_scheme <- c("#FF0000", "#FF9900", "#FFD700", "#006400", "#00008B", "#A9A9A9")
  names(color_scheme) <- cat_levels

  ################################################################################
  # I. Scatter plot #################################################################
  ################################################################################

  gg_p <- ggplot(
    data = plot_df,
    aes(
      x = plot_idx,
      y = mean,
      color = category
    )
  )

  if (error_bars == "yaxis" | error_bars == "both") {
    gg_p <- gg_p + geom_errorbar(
      data = plot_df,
      aes(x = plot_idx, ymin = lower, ymax = upper), width = 0, alpha = 0.4
    )
  }

  gg_p <- gg_p +
    geom_abline(slope = 0, intercept = c_2, color = "black", linetype = "dashed", alpha = 0.6) +
    geom_abline(slope = 0, intercept = c_3, color = "black", linetype = "dashed", alpha = 0.6) +
    geom_point(aes(size = pop), alpha = 0.7) +
    theme_classic(base_size = 16) +
    labs(
      y = paste0("Mean ", indicator_title, ": ", plot_year),
      x = xaxis_title,
      title = country_title,
      shape = "Source",
      color = "Source",
      size = pop_title
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent) +
    scale_colour_manual(name = "Category", values = color_scheme, drop = F) +
    scale_size_area(labels = scales::comma) +
    theme(legend.position = "right") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.caption = element_text(hjust = 0.5)
    ) +
    guides(
      color = guide_legend(override.aes = list(linetype = 0), order = 1),
      size = guide_legend(order = 2)
    )

  ################################################################################
  # Create a map #################################################################
  ################################################################################

  # Subset the shape
  ctry_code <- unique(plot_df$ADM0_CODE)
  plot_shape <- subset(ad2_shp, ADM0_CODE == ctry_code)
  plot_shape <- merge(plot_shape, subset(plot_df, select = c("ADM2_CODE", "category")))
  background_shape <- subset(ad0_shp, ADM0_CODE == ctry_code)

  plot_shape@data$id <- rownames(plot_shape@data)
  plot_shape_df <- fortify(plot_shape, region = "id")
  plot_shape_df <- merge(plot_shape_df, plot_shape@data, by = "id")
  plot_shape_df <- as.data.table(plot_shape_df)

  # A custom, mostly blank theme to use for mapping
  theme_empty <- theme_classic() +
    theme(
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # Make plot
  gg_map <- ggplot() +
    geom_polygon_quiet(
      data = background_shape,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "white"
    ) +
    geom_polygon_quiet(
      data = plot_shape_df,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = category
      )
    ) +
    geom_path_quiet(
      data = plot_shape,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      size = 0.2,
      color = "black"
    ) +
    geom_path_quiet(
      data = background_shape,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      size = 0.6,
      color = "black"
    ) +
    theme_empty +
    scale_size_area(max_size = max_pt_size, labels = comma) +
    coord_equal(ratio = 1) +
    scale_fill_manual(name = "Category", values = color_scheme, drop = F) +
    guides(fill = F)

  ################################################################################
  # Put it all together ##########################################################
  ################################################################################

  output_grob <- format_plot_obj_scatter(gg_map, gg_p)
  return(output_grob)
}
