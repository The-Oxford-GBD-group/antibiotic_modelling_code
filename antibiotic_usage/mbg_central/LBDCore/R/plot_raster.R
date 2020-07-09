#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ras PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION, Default: 'africa'
#' @param return_map PARAM_DESCRIPTION, Default: T
#' @param out_dir PARAM_DESCRIPTION, Default: NULL
#' @param highisbad PARAM_DESCRIPTION, Default: T
#' @param min_value PARAM_DESCRIPTION, Default: NULL
#' @param mid_value PARAM_DESCRIPTION, Default: NULL
#' @param max_value PARAM_DESCRIPTION, Default: NULL
#' @param legend_title PARAM_DESCRIPTION, Default: NULL
#' @param plot_title PARAM_DESCRIPTION, Default: NULL
#' @param layer_names PARAM_DESCRIPTION, Default: NULL
#' @param cores PARAM_DESCRIPTION, Default: 12
#' @param individual_layers PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pacman]{p_load}}
#' @rdname plot_raster
#' @export
plot_raster <- function(ras,
                        indicator,
                        region = "africa",
                        return_map = T,
                        out_dir = NULL,
                        highisbad = T,
                        min_value = NULL,
                        mid_value = NULL,
                        max_value = NULL,
                        legend_title = NULL,
                        plot_title = NULL,
                        layer_names = NULL,
                        cores = 12,
                        individual_layers = T) {

  # Inputs & Outputs ########################################################

  # Inputs:
  #     ras           = RasterBrick object (in future may extend to single layers)
  #     indicator     = your indicator name
  #     region        = region to map ('africa' only option for now)
  #     return_map    = do you want the function to return ggplot objects?
  #
  #     out_dir       = where you want the .png files to go
  #                       if null, no files returned (can use for map obj only)
  #
  #     highisbad     = are high values bad (red) in your data?
  #
  #     min_value     = minimum value for range of color bar
  #                       if not provided, minimum value in data
  #     mid_value     = inflection point for color bar
  #                       default is mean of min_value, max_value
  #     max_value     = maximum value for range of color bar
  #                       if not provided, max value in data
  #
  #     legend_title  = title for legend
  #     plot_title    = title for plot
  #     layer_names   = names for each layer of RasterBrick
  #                       most common use = year
  #
  #
  # Outputs:
  #       png files for faceted view of all layers & individual layers
  #       ggplot objects (if return_map = T) in list format
  #
  #
  # Authors: Jon Mosser (jmosser@uw.edu),
  #
  ##########################################################################

  # runtime configuration for cluster
  if (Sys.info()["sysname"] == "Linux") {
    j_root <- "/home/j/"
  } else {
    stop("Run on cluster for access to shapefiles, etc.")
  }


  # Initial checks -----------------------------------------------------------------

  # Check to see if brick or single raster
  if ("RasterBrick" %in% class(ras)) {
    is_brick <- T

    if (is.null(layer_names)) stop("Must enter layer names if plotting raster brick")
    if (nlayers(ras) != length(layer_names)) stop("Length of layer_names must match number of RasterBrick layers")
  } else {
    is_brick <- F
    stop("For now, must use RasterBrick")
  }

  # Load background map - by region ------------------------------------------------
  # For now, only support 'africa'

  message("Loading background map...")

  if (region == "africa") {
    background_shp <- readOGR(dsn = "/snfs1/temp/learl/data_coverage", layer = "africa_ad0")
  } else {
    # TODO: add other regions here in else block
    stop("Region not currently supported")
  }

  background_map <- fortify(background_shp)

  # Crop and mask data
  ras <- crop(ras, extent(background_shp))
  ras <- mask(ras, background_shp)

  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()

  map_points <- mclapply(as.list(ras),
    function(x) rasterToPoints(x) %>% as.data.table(),
    mc.cores = cores
  )

  names(map_points) <- as.character(layer_names)
  map_df <- mclapply(names(map_points),
    function(n) {
      x <- map_points[[n]]
      x <- data.table(x)
      names(x) <- c("long", "lat", "value")
      x[, layer := n]
    },
    mc.cores = cores
  )

  # Return to multithreading (if any):
  set_original_threads()

  map_df <- rbindlist(map_df)

  # Generate a range if not specified ----------------------------------------------
  if (is.null(min_value)) min_value <- min(minValue(ras)) # get global min for layers
  if (is.null(max_value)) max_value <- max(maxValue(ras)) # get global max for layers

  if (is.null(mid_value)) {
    mid_value <- rowMeans(cbind(min_value, max_value), na.rm = TRUE)
    add_mid_value <- F
  } else {
    add_mid_value <- T
  }

  # Set up color gradient ----------------------------------------------------------
  color_gradient <- c("#4575b4", "#FFFFBF", "#d73027")
  breaks <- c(min_value, mid_value, max_value)

  # Flip if high = good
  if (highisbad == F) color_gradient <- rev(color_gradient)

  # A function to generate breaks
  my_breaks <- function(x) {
    breaks <- c(min(x), mean(x), max(x))

    # Add mid_value if was originally manually specified
    if (add_mid_value == T) {
      breaks <- c(breaks, mid_value)
    }

    names(breaks) <- attr(breaks, "labels")
    breaks
  }

  # Generate a ggplot2 component for the color gradient
  color_addin <- scale_fill_gradientn(
    colors = color_gradient,
    na.value = "#a6d854",
    values = breaks,
    breaks = my_breaks,
    limits = c(min_value, max_value)
  )

  # Prep other map inputs ----------------------------------------------------------

  # Create the legend
  if (is.null(legend_title)) legend_title <- "Value"

  # Prepare layer names
  if (!is.null(layer_names)) {
    inset_df <- cbind(
      rep(-20, length(layer_names)),
      rep(-30, length(layer_names)),
      as.character(layer_names),
      as.character(layer_names)
    ) %>% as.data.table()
    names(inset_df) <- c("x", "y", "layer", "text")
    inset_df$x <- as.numeric(inset_df$x)
    inset_df$y <- as.numeric(inset_df$y)
    inset_addin <- geom_text(
      data = inset_df,
      aes(
        x = x,
        y = y,
        label = text
      ),
      size = 4,
      hjust = 0.5
    )
  } else {
    inset_addin <- NULL
  }

  # Generate the main map ----------------------------------------------------------

  # A custom, mostly blank theme
  theme_empty <- theme_classic() +
    theme(
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      text = element_text(family = "Open Sans")
    )

  # Initialize map
  g_map <- ggplot() +
    geom_polygon(
      data = background_map,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "lightgray"
    ) +
    geom_raster(
      data = map_df,
      aes(
        x = long,
        y = lat,
        fill = value
      )
    ) +
    geom_path(
      data = background_map,
      aes(x = long, y = lat, group = group),
      color = "black", lwd = .1
    ) +
    coord_equal(ratio = 1) +
    labs(
      fill = paste0(legend_title, "\n"),
      title = plot_title
    ) +
    theme_empty +
    color_addin +
    inset_addin +
    facet_wrap(~layer)

  # Outputs

  message(paste0("Saving main map for ", indicator))

  if (!is.null(out_dir)) {
    png(
      filename = paste0(out_dir, indicator, ".png"),
      type = "cairo",
      units = "in",
      width = 11,
      height = 9,
      pointsize = 12,
      res = 450
    )

    print(g_map)

    dev.off()
  }

  if (return_map) {
    map_list <- list()
    map_list[["main"]] <- g_map
  }

  # Generate individual maps -------------------------------------------------------
  split_layer_list <- lapply(layer_names, function(lyr) {
    map_df_single <- map_df[layer == lyr]
    inset_df_single <- inset_df[layer == lyr]
    return_list <- list(map_df_single, inset_df_single, lyr)
    names(return_list) <- c("map_df_single", "inset_df_single", "layer")
    return(return_list)
  })

  if (individual_layers == T) {
    # Set multithreading to serial for `mclapply()`:
    set_serial_threads()

    layer_map_list <- mclapply(split_layer_list, mc.cores = cores, FUN = function(input_list) {
      lyr <- input_list[["layer"]]
      map_df_single <- input_list[["map_df_single"]]
      inset_df_single <- input_list[["inset_df_single"]]

      inset_addin_single <- geom_text(
        data = inset_df_single,
        aes(
          x = x,
          y = y,
          label = text
        ),
        size = 10,
        hjust = 0.5
      )

      # Initialize map
      g_map_single <- ggplot() +
        geom_polygon(
          data = background_map,
          aes(
            x = long,
            y = lat,
            group = group
          ),
          fill = "lightgray"
        ) +
        geom_raster(
          data = map_df_single,
          aes(
            x = long,
            y = lat,
            fill = value
          )
        ) +
        geom_path(
          data = background_map,
          aes(x = long, y = lat, group = group),
          color = "black", lwd = .1
        ) +
        coord_equal(ratio = 1) +
        labs(
          fill = paste0(legend_title, "\n"),
          title = plot_title
        ) +
        theme_empty +
        color_addin +
        inset_addin_single

      message(paste0("Saving map for ", indicator, " layer ", lyr))

      if (!is.null(out_dir)) {
        png(
          filename = paste0(out_dir, indicator, "_", lyr, ".png"),
          type = "cairo",
          units = "in",
          width = 10,
          height = 9,
          pointsize = 12,
          res = 600
        )

        print(g_map_single)

        dev.off()
      }

      if (return_map) {
        return(g_map)
      }
    })

    # Return to multithreading (if any):
    set_original_threads()
  }

  if (return_map) {
    map_list <- c(map_list, layer_map_list)
    return(map_list)
  }
}
