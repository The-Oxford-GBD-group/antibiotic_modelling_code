# HEADER ------------------------------------------------------------------
# Author: Jonathan Mosser (jmosser@uw.edu)
# Date: 2017-11-08
# Project: Geospatial - general
# Purpose: Diagnostic functions to assess various aspects of your model
# Details: Pairs well with "prep_diagnostic_shiny.R" script
#**************************************************************************

## get_color_scheme() #####################################################

#' Grabs a color scheme from a set of defaults
#'
#' @param theme Name of theme
#' @return A vector of color hex codes
#' @examples
#' get_color_scheme("carto_discrete")

get_color_scheme <- function(theme){

  # Set up categorical colors
  carto_discrete <- c("#7F3C8D","#11A579","#F2B701","#E73F74",
                      "#3969AC","#80BA5A","#E68310","#008695",
                      "#CF1C90","#f97b72","#4b4b8f","#A5AA99",
                      "#66C5CC","#F6CF71","#F89C74","#DCB0F2",
                      "#87C55F","#9EB9F3","#FE88B1","#C9DB74",
                      "#8BE0A4","#B497E7","#D3B484","#B3B3B3")

  return(get(theme))
}

## theme_empty() ##########################################################

#' An empty `ggplot()` theme
#'
#' @return theme object for ggplot
#' @examples
#' ggplot(data = data, aes(x = x, y = y)) +
#'    geom_point() +
#'    theme_empty()

theme_empty <- function() {
  theme_empty <- theme_classic() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  return(theme_empty)
}

## plot_stacker_betas() ###################################################

#' Create a plot of stacker betas
#'
#' @param model_results table from [whatever]_model_results_table.csv
#' @param stackers vector containing names of stackers
#' @return ggplot object plotting stackers by region
#' @examples
#' # Plot stacker betas
#' gg_betas <- plot_stacker_betas(model_results = model_results,
#'                                stackers = stackers)

plot_stacker_betas <- function(model_results, stackers, xaxis = "stacker") {
  stacker_beta_table <- subset(model_results, parameter %in% stackers)
  dodge <- position_dodge(width = 0.4)
  if (xaxis == "stacker") {
    gg <- ggplot(data = stacker_beta_table, aes(x = parameter, y = q0.5, color = region)) +
      geom_point(position = dodge) +
      geom_pointrange(aes(ymin=q0.025, ymax=q0.975), position = dodge) +
      labs(x = "Stacker", y = "Beta", color = "Region") +
      scale_color_manual(values = get_color_scheme("carto_discrete")) +
      theme_classic()
  } else if (xaxis == "region") {
    gg <- ggplot(data = stacker_beta_table, aes(x = region, y = q0.5, color = parameter)) +
      geom_point(position = dodge) +
      geom_pointrange(aes(ymin=q0.025, ymax=q0.975), position = dodge) +
      labs(x = "Region", y = "Beta", color = "Stacker") +
      scale_color_manual(values = get_color_scheme("carto_discrete")) +
      theme_classic()
  }
  return(gg)
}

## plot_other_params() ####################################################

#' Create a plot of other parameters from INLA fit
#'
#' @param model_results table from [whatever]_model_results_table.csv
#' @param other_params names of other parameters (not stackers)
#' @return ggplot object plotting parametersby region
#' @examples
#' # Plot stacker betas
#' # Plot other parameters
#' gg_other_params <- plot_other_params(model_results = model_results,
#'                                      other_params = other_params)

plot_other_params <- function(model_results, other_params) {

  other_param_table <- subset(model_results, parameter %in% other_params)
  dodge <- position_dodge(width = 0.4)
  ggplot(data = other_param_table, aes(x = region, y = q0.5, color = region)) +
    geom_point(position = dodge) +
    geom_pointrange(aes(ymin=q0.025, ymax=q0.975), position = dodge) +
    labs(x = "Region", y = "Value", color = "Region") +
    facet_wrap(~parameter, scales = "free_y") +
    scale_color_manual(values = get_color_scheme("carto_discrete")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

## plot_stackers() ########################################################

#' Plot maps of stackers, mean raster of covariate, and data informing model
#'
#' @param reg region
#' @param ig indicator group
#' @param ind indicator
#' @param rd run date
#' @param ss vector of stacker names
#' @param yl year list (in vector form, e.g. `c(2000:2015)`)
#' @param zmin minimum value for color scheme (if NULL will calculate from data)
#' @param zmax maximum value for color scheme (if NULL will calculate from data)
#' @param sh_dir `/share` directory, including run date
#' @param highisbad should high values be colored in red ("bad")? Logical.
#' @param o_dir output directory
#' @param individual_countries should individual countries be graphed as well? Logical.
#' @param shapefile_version string specifies shapefile version to be used
#' @return Writes a series of image fileswith maps of each stacker, mean covariate raster, and
#'         a map of input data for each year-region combination (and year-country if individual_countries = T)
#'         in prespecified folder structure
#' @examples
#' mclapply(Regions, function(r) {
#'   message(paste0("Making stacker maps for region: ", r))
#'   plot_stackers(reg = r, highisbad = F, individual_countries = T)
#' }, mc.cores = 5)

# plot stackers & mean outcome
plot_stackers <- function(reg,
                          ig = indicator_group,
                          ind = indicator,
                          rd = run_date,
                          ss = stackers,
                          yl = year_list,
                          zmin = NULL, zmax = NULL,
                          sh_dir = sharedir,
                          highisbad = F,
                          o_dir = out_dir,
                          individual_countries = T,
                          shapefile_version = 'current') {

  # Load master shape for outlines
  # master_shape <- readRDS('/share/geospatial/rds_shapefiles/gdcv_custom/master_shape_all.rds')
  master_shape <- readRDS(get_admin_shapefile(admin_level = 0, version = shapefile_version, suffix = ".rds"))
  master_shape <- subset(master_shape, ADM0_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version))

  # Set up output dir
  o_dir <- paste0(o_dir, "/stacker_maps/")
  dir.create(o_dir, recursive = T, showWarnings = F)

  # Load stacker objects
  load(paste0("/share/geospatial/mbg/", ig, "/", ind, "/model_image_history/",
              rd, "_bin0_", reg, "_0.RData"))
  # subset cov_list to stackers
  stacker_list <- cov_list[which(names(cov_list) %in% ss)]


  # load mean unraked raster for estimates. Try region rasters first; whole raster & crop if not.
  if (file.exists(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else if (file.exists(paste0(sh_dir, ind, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else {
    stop("Could not find unraked raster .tif.")
  }

  # add mean raster to stacker list
  stacker_list[[ind]] <- result_brick

  # load input data from csv if present; re-generate if needed
  if (file.exists(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"))) {
    input_df <- read.csv(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"),
                         stringsAsFactors=F) %>%
                as.data.table
  } else {
    gaul_list <- get_adm0_codes(reg,
                                shapefile_version = shapefile_version)
    simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list,
                                               buffer = 0.4,
                                               shapefile_version = shapefile_version)
    subset_shape   <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    raster_list    <- build_simple_raster_pop(subset_shape)
    simple_raster  <- raster_list[['simple_raster']]
    pop_raster     <- raster_list[['pop_raster']]

    input_df <-  load_input_data(indicator = indicator,
                                 simple = simple_polygon,
                                 removeyemen = TRUE,
                                 yl = yl)
  }

  input_df[, outcome := get(indicator) / N]

  # Define function to save maps
  save_maps <- function(input_df, stacker_list, master_shape,
                        result_brick, zmin, zmax, yl, ind, ig, sh_dir,
                        highisbad, o_dir, ctry = NULL,
                        shapefile_version) {

    if (!is.null(ctry)) {
      input_df <- subset(input_df, country == ctry)
      master_shape <- subset(master_shape, ADM0_CODE == get_adm0_codes(ctry, shapefile_version = shapefile_version))
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
    if (is.null(zmin)) zmin <- min(sapply(stacker_list, function(x) min(minValue(x))),
                                   min(input_df$outcome))
    if (is.null(zmax)) zmax <- max(sapply(stacker_list, function(x) max(maxValue(x))),
                                   max(input_df$outcome))

    # Check # years correct
    if (nlayers(result_brick) != length(yl)) stop("Number of mean raster brick layers does not equal length of year list")

    # rearrange
    for (i in 1:length(yl)) {

      message(paste0("   year ", yl[i], "..."))

      make_gg_map <- function(rbrick, title, i) {
        r_df <- rbrick[[i]] %>% as("SpatialPixelsDataFrame") %>% as.data.frame
        names(r_df) <- c("value", "x", "y")
        gg_result <-ggplot() +
          geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
          coord_equal() +
          theme_empty() +
          labs(title = title) +
          scale_fill_distiller(palette = "RdYlBu",
                               direction = ifelse(highisbad, -1, 1),
                               limits = c(zmin,zmax)) +
          geom_path(data = master_shape_df,
                    aes(x = long, y = lat, group = group))
        return(gg_result)
      }

      gg_stackers_results <- lapply(1:length(stacker_list), function(n) {
        the_title <- paste0(names(stacker_list)[n], ": ", yl[i])
        the_rbrick <- stacker_list[[n]]
        return(make_gg_map(the_rbrick, the_title, i))
      })

      # Make data plot
      gg_data <- ggplot() +
        geom_point(data = subset(input_df, year == yl[i] & weight < 1),
                   aes(x = longitude, y = latitude, size = cap_N, alpha = weight, color = outcome)) +
        geom_point(data = subset(input_df, year == yl[i] & weight == 1),
                   aes(x = longitude, y = latitude, size = cap_N, color = outcome)) +
        coord_equal() +
        theme_empty() +
        scale_color_distiller(palette = "RdYlBu",
                              direction = ifelse(highisbad, -1, 1),
                              limits = c(zmin,zmax))+
        geom_path(data = master_shape_df,
                  aes(x = long, y = lat, group = group)) +
        #  scale_alpha(range = c(0,1)) +
        scale_size_continuous(limits = c(NA, max(input_df$cap_N))) +
        labs(title = paste0("data: ", yl[i]))

      gg_stackers_results[[length(gg_stackers_results) + 1]] <- gg_data

      # Use first legend only
      the_legend <- g_legend(gg_stackers_results[[1]])
      gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position="none")))

      if (is.null(ctry)) {
        reg_dir <- paste0(o_dir, reg, "/")
        dir.create(reg_dir, recursive = T, showWarnings = F)
        fn <-paste0(reg_dir, "stacker_map_", reg, "_", yl[i], ".png")
      } else if (!is.null(ctry)) {
        ctry_dir <- paste0(o_dir, ctry, "/")
        dir.create(ctry_dir, recursive = T, showWarnings = F)
        fn <- paste0(ctry_dir, "stacker_map_", ctry, "_", yl[i], ".png")
      }

      png(filename = fn,
          width = 16, height = 9, units = "in",
          res = 200, pointsize = 10,
          type = "cairo-png")

      multiplot(plotlist = gg_stackers_results,
                cols = ceiling(length(gg_stackers_results) / 2),
                legend = the_legend)

      dev.off()
    }
  }

  # Save map for entire region
  save_maps(input_df, stacker_list, master_shape, result_brick, zmin,
            zmax, yl, ind, ig, sh_dir, highisbad, o_dir, ctry = NULL,
            shapefile_version = shapefile_version)

  # Save maps for individual countries
  if (individual_countries == T) {
    gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
    gaul_list <- data.table(GAUL_CODE = get_adm0_codes(reg, shapefile_version = shapefile_version))
    gaul_list <- merge(gaul_list, gaul_to_loc_id, by = "GAUL_CODE")
    for (c in unique(gaul_list$ihme_lc_id)) {
      if (!(get_adm0_codes(c, shapefile_version = shapefile_version) %in% master_shape$ADM0_CODE)) {
        message(paste0("No shapes in master_shape corresponding to admin 0 code for ", c, " - skipping..."))
      } else {
        message(paste0("Saving stacker maps for country: ", c))
      save_maps(input_df, stacker_list, master_shape, result_brick, zmin, zmax, yl, ind, ig, sh_dir, highisbad, o_dir, ctry = c, shapefile_version = shapefile_version)
      }  
    }
  }
}

## g_legend() #############################################################

#' Pulls legend out of a ggplot object for use later
#'
#' based off of function originally written by Hadley Wickham
#'
#' @param a.gplot ggplot() object
#' @return grob containing legend
#' @examples
#' the_legend <- g_legend(my_gplot)

g_legend<-function(a.gplot){
  pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  graphics.off()
  return(legend)
}

## plot_gam_models ########################################################

#' Creates plots of component smooth objects from fitted gam model
#'
#' This function creates plots in a standardized 4x3 format, saved to
#' standardized directories and filenames
#'
#' @param child_model_list Nested list of child model objects (models within regions)
#'                         such that child_model_list[["cssa"]][["gam"]] returns the
#'                         appropriate gam model object for region cssa
#' @param regions character vector of regions
#' @param o_dir output directory
#' @return writes png files in standardized format to `o_dir\gam\[region]\`
#' @examples
#' message("Loading child models for each region")
#' child_model_list <- lapply(Regions, function(reg) {
#'   child_model_file <- paste0(sharedir, "child_model_list_", reg, "_0.RData")
#'   load(child_model_file, verbose = F)
#'   return(child_models)
#' })
#'
#' names(child_model_list) <- Regions
#'
#' # Plot GAM models
#' plot_gam_models(child_model_list = child_model_list,
#'                 regions = Regions,
#'                 o_dir = out_dir)

plot_gam_models <- function(child_model_list, regions, o_dir) {

  # Create gam plots in a standardized output format (3x3 grid) for each
  # of the child models (for each region)

  str_match <- stringr::str_match

  for (reg in regions) {

    # Set up directory
    dirname <- paste0(o_dir, "gam/", reg, "/")
    dir.create(dirname, recursive = T, showWarnings = F)

    # Load child model
    gam_child <- child_model_list[[reg]][["gam"]]
    labs <- sapply(gam_child$smooth, function(x) return(x$label))
    n_plots <- length(labs)

    # Create list of which plots go in which pages
    x <- 1:n_plots
    length(x) <- suppressWarnings(prod(dim(matrix(x, ncol = 12, byrow = T))))
    x <- matrix(x, ncol = 12, byrow = T)

    for (i in 1:nrow(x)) {

      # Set up each page (row in layout matrix) & plot

      terms <- x[i,]
      terms <- terms[!is.na(terms)]

      filename <- paste0(dirname, "gam_plot_", reg, "_page_", i, ".png")

      png(filename=filename,
          type="cairo",
          units="in",
          width=12,
          height=8,
          pointsize=14,
          res=200)

      par(mfrow = c(3,4))
      for (z in terms) {
        plot(gam_child, select = z)
      }

      dev.off()
    }
  }
}

## multiplot() ############################################################

#' Function to plot multiple ggplot objects together
#'
#' Adapted from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param ... Can pass in ggplot objects in `...` or in plotlist
#' @param plotlist Can pass in ggplot objects as a list with this argument instead of in `...`
#' @param cols Number of columns in layout
#' @param layout Matrix specifying the layout; i.e. `matrix(c(1,2,3,3), nrow = 2, byrow = T)`.
#'               If `layout` is specified, then `cols` is ignored
#' @param legend A legend object.  If legend is passed, then this will add an extra cell at the
#'               end of the grid layout and insert the legend there (good, for instance, if you
#'               have common legends for all of your plots and only want to show it once).
#' @return Prints a gridded output of your ggplot objects to the active graphical device
#' @examples
#' # Note: gg_stackers_results is a list of ggplot objects
#'
#' # Use first legend only
#' the_legend <- g_legend(gg_stackers_results[[1]])
#' gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position="none")))
#'
#' multiplot(plotlist = gg_stackers_results,
#'           cols = ceiling(length(gg_stackers_results) / 2),
#'           legend = the_legend)



multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, legend = NULL) {

  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)
  if (!is.null(legend)) numPlots <- numPlots + 1 # add legend

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),
                     byrow = T)
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:(numPlots-1)) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }

    # Print the legend if present
    if (!is.null(legend)) {
      matchidx <- as.data.frame(which(layout == numPlots, arr.ind = T))
      legend$vp <- viewport(layout.pos.row = matchidx$row,
                            layout.pos.col = matchidx$col)
      grid.draw(legend)
    }
  }
}

#
## make_model_diagnostics() --------------------------------------------------->
#'
#' @title Function to submit a qsub to run the model_diagnostics.R script
#'
#' @description \code{make_model_diagnostics} creates a qsub string to run the '
#'   model_diagnostics.R script on the cluster and does the system call to actually
#'   submit the job
#'
#' @param code Name of script, with relative path if desired.
#' 
#' @param code_path Full path to R script. Overrides \code{code} and \code{script_dir}
#' 
#' @param cores Number of threads. Default: 5.
#' 
#' @param memory RAM to be reserved, in GBs
#'
#' @param geo_nodes If TRUE, your job will be submitted to the geos (LBD)
#'   cluster, if FALSE, it will be submitted to the prod cluster. Note that if
#'   using the 'proj' argument, make sure to use project name which is valid on
#'   the cluster you are submitting to. [default = FALSE]
#'
#' @param use_c2_nodes If TRUE, your job will be submitted to the C2 nodes on
#'   the prod cluster, if FALSE, the C2 nodes are not specified. Note that if
#'   FALSE, your job may land on a node with much less memory or your node may
#'   still land on a C2 node anyway. If both the 'use_c2_nodes' and 'geo_nodes'
#'   arguments are set to TRUE, then the code will issue a warning and default
#'   to the geos nodes. [default = FALSE]
#'
#' @param proj Can pass in a project name to submit your job under. If default
#'   and the 'geo_nodes' argument is left as its default of 'FALSE', jobs
#'   will be submitted to the prod cluster under the default project
#'   'proj_geospatial'. If default and with 'geos_nodes = TRUE', jobs will be
#'   submitted to the geos (LBD) nodes under the default project
#'   'proj_geo_nodes'. If a project name is passed in for 'proj' the job will
#'   be submitted under that project. Note that this function does not check for
#'   valid project names since these are likely to change often and likely
#'   valid project names are different on each cluster. [default = NULL]
#'
#' @param queue Queue to be used on the fair cluster.
#'
#' @param run_time Run-time to be used on the fair cluster.
#'
#' @param priority Job priority that can be deprioritized if needed, and can only be used for values in [-1023,0]. Default = 0.
#' This value will get bounded to 0 or -1023 if the user supplies a value outside those bounds.
#'
#' @param singularity Instead of using the default R installation on the geos
#'   or prod nodes, launch R from a Singularity image. This arg currently takes
#'   three options: the default is NULL, indicating not to launch a Singularity
#'   container, 'default' if you wish to launch a Singularity container from the
#'   default image, or you can provide a string which can be either a complete
#'   path to a Singularity image that is not located at the default image
#'   location, or just the name of the Singularity image that is assumed located
#'   at the default image location.  If 'default' is chosen, the default image
#'   is defined in the shell script executed by this R script ('shell_sing.sh')
#'   so that no R code need be updated when the default image is updated.
#'   Different versions of a Singularity image or test versions may be specified
#'   by providing the name or path of the image. Currently, all standard images
#'   for LBD are kept at the default location of /share/singularity-images/lbd.
#'   [default = NULL]
#' @param singularity_opts pass in a named list of environmental variables.
#'   \code{qsub_sing_envs} will check that the names of the list members passed
#'   in match the environmental variables that the shell_sing.sh script knows
#'   about: 'SET_OMP_THREADS' and/or 'SET_MKL_THREADS'. Passing in other
#'   environmental names in the list will result in an error. If this is left
#'   as 'NULL' and a Singularity image is used, SET_OMP_THREADS and
#'   SET_MKL_THREADS will remain unset and the shell_sing.sh script will use
#'   the default setting of SET_OMP_THREADS=1 and SET_MKL_THREADS={max_threads}
#'   (see shell_sing.sh comments). For example SET_OMP_THREADS=1 and
#'   SET_MKL_THREADS=4 can be achieved by passing in
#'     \code{envs = list(SET_OMP_THREADS=1, SET_MKL_THREADS=4)}
#'   [default = list(SET_OMP_THREADS = cores, SET_MKL_THREADS = cores)<Paste>]
#'
#' @return None
#'
#' @seealso This function uses the following functions found in
#'   mbg_central/misc_functions.R:
#'   \code{\link{get_singularity}}
#'   \code{\link{qsub_sing_envs}}
#'
#' @export
#'
make_model_diagnostics <- function(user = Sys.info()["user"],
                                   code_path = NULL,
                                   cores = 5,
                                   memory = 10,
                                   proj = NULL,
                                   ig = indicator_group,
                                   corerepo = core_repo,
                                   indic = indicator,
                                   rd = run_date,
                                   log_location = "sgeoutput",
                                   code = "model_diagnostics",
                                   script_dir = "mbg_central/share_scripts",
                                   keepimage = FALSE,
                                   shell = "r_shell.sh",
                                   geo_nodes = FALSE,
                                   use_c2_nodes = FALSE,
                                   queue = NULL,
                                   run_time = NULL,
                                   priority = 0,
                                   singularity = singularity_version,
                                   singularity_opts = list(SET_OMP_THREADS = cores, SET_MKL_THREADS = cores)) {
  # Define project first (necessary to validate node options)
  proj <- get_project(proj, use_geo_nodes = geo_nodes)
  
  # Validate arguments
  validate_singularity_options(singularity, singularity_opts)
  validate_node_option(geo_nodes, use_c2_nodes, proj)
  
  temp_dir <- path_join(get_model_output_dir(ig, indic, rd), "temp_post_est")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Determine where stdout and stderr files will go
  output_err <- setup_log_location(log_location, user, indic, ig, rd)
  output_log_dir <- output_err[[1]]
  error_log_dir <- output_err[[2]]
  
  # Since we no longer rely on `setwd()`'s, we need to construct a sensible
  # "script_dir". If someone wants to use a special script, we assume it is
  # somewhere in their corerepo here:
  script_dir <- path_join(corerepo, script_dir)
  code <- path_join(script_dir, paste0(code, ".R"))
  
  # If code_path is not NULL, then override `code`
  if(!is.null(code_path)) {
    code <- code_path
  }
  
  # Define remaining job attributes
  job_name <- paste0("job_dx_", indic)
  run_time <- get_run_time(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
  queue <- get_queue(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
  shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_sing.sh")
  sing_image <- get_singularity(image = singularity)
  
  # resources are all the -l qsub arguments
  resources <- get_resources(use_geo_nodes = geo_nodes, cores = cores, ram_gb = memory, runtime = run_time)
  
  qsub <- generate_qsub_command(
    # qsub-specific arguments
    stderr_log = error_log_dir,
    stdout_log = output_log_dir,
    project = proj,
    resources = resources,
    job_name = job_name,
    singularity_str = qsub_sing_envs("", singularity_opts, sing_image),
    cores = cores,
    queue = queue,
    priority = priority,
    # Command to qsub
    shell, code, indic, ig, rd, cores, corerepo
  )
  
  # make the qsub call
  system(qsub)
}

