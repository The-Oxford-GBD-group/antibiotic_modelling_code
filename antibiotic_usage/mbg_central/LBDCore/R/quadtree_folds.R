#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION
#' @param mb PARAM_DESCRIPTION
#' @param ts PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param plot_fn PARAM_DESCRIPTION, Default: NULL
#' @param plot_shp PARAM_DESCRIPTION, Default: NULL
#' @param save_qt PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @param t_folds PARAM_DESCRIPTION, Default: 1
#' @param stratum PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname quadtree_folds
#' @export
quadtree_folds <- function(xy, ## 2 col matrix of xy locs
                           ss, ## vector of sample size at each loc - if all 1s, it aggregates by # of points
                           mb, ## minimum allowed pts in a quadtree region
                           ts, ## target sample size in each region
                           n_folds, ## number of folds,
                           plot_fn = NULL, ## if true plots data and quad tree must be .png
                           plot_shp = NULL, ## to add shapefile outlines to plot
                           save_qt = TRUE, ## if desired, can save quadtree regions to shapefiles
                           ...,
                           t_folds = 1, ## if multiple t_folds. to save shapefiles
                           stratum = 1 ## for multiple stratum to save shapefiles
                           #                           pathaddin = ""   ## file path addin to specifiy specifis of model run
                           #                           run_date = run_date
) {



  ##############
  ## IN SPACE ##
  ##############

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (1) random in space.
  ## this is the same as totally random with one time point
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (2) small aggregates in space - QUADTREE
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ## example
  ## df <- fread('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',
  ##            stringsAsFactors = FALSE)
  ## clean the df
  ## df$long <- as.numeric(as.character(gsub(",", "", df$long)))
  ## df$lat  <- as.numeric(as.character(gsub(",", "", df$lat)))
  ## df <- df[-which(df$lat > 90), ]
  ## data <- df
  ## shp_full <- plot_shp <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')
  ## xy <- data[,c('long', 'lat')]
  ## ss <- data$exposed
  ## ts <- 1e6
  ## plot_fn <- 'quadtree.png'
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


  ## this function segregates data into spatial regions. it splits
  ## data down until the target sample size is reached or until a
  ## minimum allowable number of points are in the bin

  ## it then breaks the data into folds by ensuring that all data in
  ## any one spatial region stays together and such that the sample
  ## size sums in each fold are relatively similar

  ## points at the same place pose a problem because you can't split them
  ## so, first we make a subset of the data using only unique xy combos
  ## quadtree is run on the unique list and then we buid back up to
  ## the full dataset and make folds

  ## make data.table
  dt_xy <- data.table(cbind(xy, ss))

  ## round to 5 decimal points to ensure matching
  cols <- names(dt_xy)[1:2]
  dt_xy[, (cols) := round(.SD, 5), .SDcols = cols]

  ## griyo by lat and long combos
  dt_xy <- dt_xy[, loc_id := .GRP, by = c("long", "lat")]

  ## unique xy locations by group and sum ss in groups
  un_xy <- dt_xy[, list(long = mean(long), lat = mean(lat), ss = sum(ss)), by = c("loc_id")]

  ## to allow for deeper recursions in quadtree
  options(expressions = 500000)

  ## get quad-tree regions on unique locations and ss sum at those locations
  system.time(qt <- quadtree_ct(
    xy = as.matrix(un_xy[, .(long, lat)]),
    ss = as.numeric(un_xy[, ss]),
    target_ss = ts,
    min_in_bin = 1,
    rand = T
  ))

  ## in order to match these back to all the points, we collect locations and ids
  ## this can take a little while...
  ids <- id(qt)
  ids <- na.omit(ids) ## qt alg adds a few NA rows sometimes...

  ## now we match ids to our original dataset
  ## TODO: make this faster somehow... Roy suggested a forwardfill option
  dt_xy[, row := 1:nrow(dt_xy) ]
  setkey(dt_xy, long, lat)
  dt_xy[, qt_id := -1.0]
  system.time(
    for (i in 1:nrow(ids)) {
      ## this next line is to fix weird dropping 0 issues resulting in non-matches
      loc <- data.table(long = round(ids[i, 2], 6), lat = round(ids[i, 3], 6))
      dt_xy[.(loc), qt_id := ids[i, 1] ]
    }
  )

  ## now we stratify - selecting by qt_id and ensuring sum of ss in
  ## folds is close to even across folds
  if (length(unique(dt_xy[, qt_id])) < n_folds) {
    message("Warning: Fewer quadtree sections than n_folds!! Things may break. Either increase data amount or decrease ts")
  }

  cts_in_qt <- dt_xy[, sum(ss), by = qt_id]
  fold_vec <- make_folds_by_poly(
    cts_in_polys = as.matrix(cts_in_qt),
    pt_poly_map = as.numeric(dt_xy[, qt_id]),
    n_folds = n_folds
  )

  ## now we 'unsort' dt_xy to make it match the orde of the original data
  dt_xy[, fold_vec := fold_vec]
  setkey(dt_xy, row)
  fold_vec <- dt_xy[, fold_vec]
  ho_id <- dt_xy[, qt_id]

  ## plot if desired
  if (!is.null(plot_fn)) {
    xylim <- cbind(x = c(min(xy[, 1]), max(xy[, 1])), y = c(min(xy[, 2]), max(xy[, 2])))
    png(paste0(plot_fn), width = 4000, height = 4000)
    title <- paste0("Quadtree with threshold of ", ts)
    if (!is.null(plot_shp)) {
      plot(plot_shp, xlab = "x", ylab = "y", main = title)
    } else {
      plot(xylim, type = "n", xlab = "x", ylab = "y", main = title)
    }
    lines(qt, xylim, col = "Gray")
    cols <- hsv(fold_vec / n_folds, 1, 1)

    points(dt_xy[, .(long, lat)], col = alpha(cols, alpha = 0.5), pch = 16, cex = 2)
    dev.off()
  }

  ## save quadtree rectangles to shapefile if desired
  if (save_qt) {
    #    if(time_stamp==TRUE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
    #    if(time_stamp==FALSE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/scratch')
    #    dir.create(output_dir, showWarnings = FALSE)
    output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
    qt_shape_dir <- paste0(output_dir, "/holdout_shapefiles/")
    dir.create(qt_shape_dir)

    xylim <- cbind(x = c(min(un_xy[, 2]), max(un_xy[, 2])), y = c(min(un_xy[, 3]), max(un_xy[, 3])))
    polys <- cell(qt, xylim)
    polys_attr <- data.frame(id = unique(polys$id))
    polys_shapefile <- convert.to.shapefile(polys, polys_attr, "id", 5)
    write.shapefile(polys_shapefile, paste0(qt_shape_dir, "spat_holdout_stratum_", stratum, "_t_fold", t_folds), arcgis = TRUE)
  }

  return(cbind(fold_vec, ho_id))
}
