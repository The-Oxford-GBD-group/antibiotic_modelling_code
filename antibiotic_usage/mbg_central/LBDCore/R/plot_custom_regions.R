#' @title Plot the world with custom region lists
#' @description Make a world plot of custom region lists
#'
#' @param region_list vector of region names (defined in `pull_custom_modeling_regions()`)
#' @return ggplot object of the map of the custom regions
#' @examples
#' \dontrun{
#' png(
#'   file = "/path/to/file.png",
#'   width = 14,
#'   height = 6,
#'   units = "in",
#'   res = 300
#' )
#' 
#' plot_custom_regions(region_list, plot_title = "Vaccine Regions")
#' 
#' dev.off()
#' }
#' @export
plot_custom_regions <- function(region_list,
                                plot_title = NULL,
                                verbose = T,
                                shapefile_version = "current") {

  # Replace some functions to be quieter
  geom_polygon_quiet <- function(...) {
    suppressMessages(ggplot2::geom_polygon(...))
  }
  geom_path_quiet <- function(...) {
    suppressMessages(ggplot2::geom_path(...))
  }

  # Function to pull a table of gauls by region
  make_gaul_table <- function(rr) {
    data.table(
      region = rr,
      gaul_code = get_adm0_codes(rr, shapefile_version = shapefile_version)
    )
  }

  reg_table <- lapply(region_list, make_gaul_table) %>% rbindlist()

  # Add in stage 1/2 countries that aren't in custom list
  eval_adm0_codes <- get_adm0_codes(c("stage1", "stage2"), shapefile_version = shapefile_version)
  default_reg_table <- data.table(
    in_s1s2 = T,
    gaul_code = eval_adm0_codes
  )

  reg_table <- merge(reg_table, default_reg_table, all.x = T, all.y = T)
  reg_table[is.na(region) & in_s1s2 == T, region := "missing"]

  # Load gaul shapefile
  if (verbose == T) message("Loading shapefiles...")
  world_shape <- readRDS("/share/geospatial/rds_shapefiles/simplified_shapefiles/g2015_2014_0_simp_tol_0.1.rds")
  world_shape <- subset(world_shape, ADM0_NAME != "Antarctica")
  world_shape <- merge(world_shape, unique(reg_table), all.x = T, all.y = F, by.x = "ADM0_CODE", by.y = "gaul_code")

  # Make the map

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

  # Discrete color palette from Carto colors
  carto_discrete <- c(
    "#7F3C8D", "#11A579", "#F2B701", "#E73F74",
    "#3969AC", "#80BA5A", "#E68310", "#008695",
    "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99",
    "#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2",
    "#87C55F", "#9EB9F3", "#FE88B1", "#C9DB74",
    "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3"
  )

  color_scheme <- carto_discrete[1:length(region_list)]
  names(color_scheme) <- region_list

  # Fortify once
  if (verbose == T) message("Fortifying shapefile...")
  world_shape@data$id <- rownames(world_shape@data)
  world_shape_df <- suppressMessages(as.data.table(fortify(world_shape, region = "id")))
  world_shape_df <- merge(world_shape_df, as.data.table(world_shape@data))

  # Get centroids for small areas in order to overlay circles
  if (verbose == T) message("Calculating centroids for small countries...")
  small_shapes <- subset(world_shape, area(world_shape) < 1e11)

  cents <- gCentroid(small_shapes, byid = T, id = small_shapes$id) %>%
    as.data.frame() %>%
    cbind(rownames(.), .) %>%
    as.data.table() %>%
    setnames(., names(.), c("id", "x", "y"))

  cents <- merge(cents, small_shapes@data, by = "id") %>% as.data.table()

  cents <- subset(cents, !is.na(region) | (in_s1s2 == TRUE))

  # Fix Kiribati if present -- spans 0 long so centroid wraps to close to 0,0
  cents[ADM0_CODE == 135, x := -154.75583729] # http://www.marineregions.org/gazetteer.php?p=details&id=8441
  cents[ADM0_CODE == 135, y := -3.81449579] # http://www.marineregions.org/gazetteer.php?p=details&id=8441

  # Make map and add on options
  if (verbose == T) message("Drawing map...")
  gg_map <- ggplot() +
    geom_polygon_quiet(
      data = world_shape_df,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "gray"
    )

  if (length(subset(world_shape_df, region == "missing")) > 0) {
    gg_map <- gg_map + geom_polygon_quiet(
      data = subset(world_shape_df, region == "missing"),
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "white"
    )
  }

  # Plot regions by color and overlay borders
  gg_map <- gg_map +
    geom_polygon_quiet(
      data = subset(world_shape_df, region != "missing" & !is.na(region)),
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      )
    ) +
    geom_path_quiet(
      data = world_shape_df,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      size = 0.2,
      color = "black"
    )

  # If small geographies present, overlay a circle for easier visualization
  if (nrow(cents) > 0) {
    gg_map <- gg_map +
      geom_point(
        data = cents,
        aes(x = x, y = y, color = region),
        size = 4,
        alpha = 0.5
      )
  }

  # Add on plot options including title if needed
  gg_map <- gg_map +
    theme_empty +
    coord_equal(ratio = 1) +
    scale_fill_manual(name = "Region", values = color_scheme) +
    scale_color_manual(name = "Region", values = color_scheme) +
    guides(color = FALSE)

  if (!is.null(plot_title)) gg_map <- gg_map + labs(title = plot_title)

  # Return the final map
  return(gg_map)
}
