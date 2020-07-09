#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param input_df PARAM_DESCRIPTION
#' @param stacker_list PARAM_DESCRIPTION
#' @param master_shape PARAM_DESCRIPTION
#' @param result_brick PARAM_DESCRIPTION
#' @param zmin PARAM_DESCRIPTION
#' @param zmax PARAM_DESCRIPTION
#' @param yl PARAM_DESCRIPTION
#' @param ind PARAM_DESCRIPTION
#' @param ig PARAM_DESCRIPTION
#' @param sh_dir PARAM_DESCRIPTION
#' @param highisbad PARAM_DESCRIPTION
#' @param o_dir PARAM_DESCRIPTION
#' @param ctry PARAM_DESCRIPTION, Default: NULL
#' @param shapefile_version PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname save_maps
#' @export
save_maps <- function(input_df, stacker_list, master_shape,
                      result_brick, zmin, zmax, yl, ind, ig, sh_dir,
                      highisbad, o_dir, ctry = NULL,
                      shapefile_version) {
  if (!is.null(ctry)) {
    input_df <- subset(input_df, country == ctry)
    eval_adm0_codes <- get_adm0_codes(ctry, shapefile_version = shapefile_version)
    master_shape <- subset(master_shape, ADM0_CODE == eval_adm0_codes)
    stacker_list <- lapply(stacker_list, function(x) {
      x <- suppressMessages(crop(x, extent(master_shape)))
      x <- suppressMessages(mask(x, master_shape))
      extent(x) <- extent(master_shape)
      return(x)
    })
  }

  master_shape_df <- suppressMessages(fortify(master_shape))

  # Cap N at 90% for plot interpretability
  input_df[, cap_N := ifelse(N >= quantile(N, 0.9), quantile(N, 0.9), N)]

  # define mins / maxes from global values across all years in stacker_list & input data
  if (is.null(zmin)) {
    zmin <- min(
      sapply(stacker_list, function(x) min(minValue(x))),
      min(input_df$outcome)
    )
  }
  if (is.null(zmax)) {
    zmax <- max(
      sapply(stacker_list, function(x) max(maxValue(x))),
      max(input_df$outcome)
    )
  }

  # Check # years correct
  if (nlayers(result_brick) != length(yl)) stop("Number of mean raster brick layers does not equal length of year list")

  # rearrange
  for (i in 1:length(yl)) {
    message(paste0("   year ", yl[i], "..."))


    gg_stackers_results <- lapply(1:length(stacker_list), function(n) {
      the_title <- paste0(names(stacker_list)[n], ": ", yl[i])
      the_rbrick <- stacker_list[[n]]
      return(make_gg_map(the_rbrick, the_title, i))
    })

    # Make data plot
    gg_data <- ggplot() +
      geom_point(
        data = subset(input_df, year == yl[i] & weight < 1),
        aes(x = longitude, y = latitude, size = cap_N, alpha = weight, color = outcome)
      ) +
      geom_point(
        data = subset(input_df, year == yl[i] & weight == 1),
        aes(x = longitude, y = latitude, size = cap_N, color = outcome)
      ) +
      coord_equal() +
      theme_empty() +
      scale_color_distiller(
        palette = "RdYlBu",
        direction = ifelse(highisbad, -1, 1),
        limits = c(zmin, zmax)
      ) +
      geom_path(
        data = master_shape_df,
        aes(x = long, y = lat, group = group)
      ) +
      #  scale_alpha(range = c(0,1)) +
      scale_size_continuous(limits = c(NA, max(input_df$cap_N))) +
      labs(title = paste0("data: ", yl[i]))

    gg_stackers_results[[length(gg_stackers_results) + 1]] <- gg_data

    # Use first legend only
    the_legend <- g_legend(gg_stackers_results[[1]])
    gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position = "none")))

    if (is.null(ctry)) {
      reg_dir <- paste0(o_dir, reg, "/")
      dir.create(reg_dir, recursive = T, showWarnings = F)
      fn <- paste0(reg_dir, "stacker_map_", reg, "_", yl[i], ".png")
    } else if (!is.null(ctry)) {
      ctry_dir <- paste0(o_dir, ctry, "/")
      dir.create(ctry_dir, recursive = T, showWarnings = F)
      fn <- paste0(ctry_dir, "stacker_map_", ctry, "_", yl[i], ".png")
    }

    png(
      filename = fn,
      width = 16, height = 9, units = "in",
      res = 200, pointsize = 10,
      type = "cairo-png"
    )

    multiplot(
      plotlist = gg_stackers_results,
      cols = ceiling(length(gg_stackers_results) / 2),
      legend = the_legend
    )

    dev.off()
  }
}
