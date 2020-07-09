#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION, Default: df
#' @param smooth PARAM_DESCRIPTION, Default: 1
#' @param age_col PARAM_DESCRIPTION, Default: 'age'
#' @param yr_col PARAM_DESCRIPTION, Default: 'year'
#' @param died_col PARAM_DESCRIPTION, Default: 'died'
#' @param N_col PARAM_DESCRIPTION, Default: 'N'
#' @param weight_col PARAM_DESCRIPTION, Default: 'weight'
#' @param long_col PARAM_DESCRIPTION, Default: 'longitude'
#' @param lat_col PARAM_DESCRIPTION, Default: 'latitude'
#' @param dt_col PARAM_DESCRIPTION, Default: 'data_type'
#' @param save_dir PARAM_DESCRIPTION, Default: '~'
#' @param dim PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[base]{sample}}
#' @rdname plot_5q0
#' @export
plot_5q0 <- function(data = df,
                     smooth = 1,
                     age_col = "age",
                     yr_col = "year",
                     died_col = "died",
                     N_col = "N",
                     weight_col = "weight",
                     long_col = "longitude",
                     lat_col = "latitude",
                     dt_col = "data_type", ## data type col
                     save_dir = "~",
                     dim = 10 ## height/width of pdf images
) {


  ###################################################################################
  ## this function is meant to plot your raw data (or something as close
  ## to raw as possible) in a format that closely resembles your mbg
  ## model output for comparison
  ##
  ## Inputs -
  ##   data:    cleaned data.frame that will go into MBG run
  ##   *_col:   string name of relevant columns in data
  ##   smooth:  tuning for thin-plate spline smoothness
  ##   raster_tmp: raster template to match its grid
  ##
  ## Output - layered raster brick/stack. different layers for different time periods
  ###################################################################################

  ## get the dataframe we need set up
  d <- as.data.frame(data) ## for indexing carefulness
  d[[died_col]] <- d[[died_col]] * d[[weight_col]]
  d[[N_col]] <- d[[N_col]] * d[[weight_col]]
  d <- d[, c(long_col, lat_col, yr_col, age_col, died_col, N_col, dt_col)]
  yrs <- sort(unique(d[[yr_col]]))

  if (FALSE) { ## for interpolating onto raster locations vvvvv
    ## load the population raster - we'll match the cells it uses
    ## must be on cluster!
    ## also, pull out info about the raster we'll use to predict and make the raster object
    pop <- brick("/home/j/temp/geospatial/U5M_africa/data/raw/covariates/new_20160421/pop_stack.tif")
    ## Convert raster to SpatialPointsDataFrame
    r.pts <- rasterToPoints(pop, spatial = TRUE)
    proj4string(r.pts)

    ## reproject sp object
    geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    r.pts <- spTransform(r.pts, CRS(geo.prj))
    proj4string(r.pts)

    ## Assign coordinates to @data slot, display first 6 rows of data.frame
    r.pts@data <- data.frame(r.pts@data,
      long = coordinates(r.pts)[, 1],
      lat = coordinates(r.pts)[, 2]
    )
    head(r.pts@data)
    ## make a matrix of locations where we want to project our smoothed estimate
    proj.ll <- r.pts@data[, 5:6]
  }

  ## loop through the years and make a raster for each year
  fq0_list_cbh <- list(NULL)
  fq0_list_sbh <- list(NULL)
  fq0_list <- list(NULL)


  #  for(dt in c("cbh", "sbh")){
  #    print(paste0("~~~ ON DATA TYPE: ", dt))
  for (y in yrs) {
    print(paste0("On year: ", y))

    dy <- d[which(d[[yr_col]] == y), ] # & d[[dt_col]] == dt), ] ## get the subset of the data for the year and data type

    ## aggregate died_col by lat-long location and combine aggregates into one frame
    f.dyd <- formula(paste0(died_col, "~", long_col, "+", lat_col, "+", age_col))
    dya <- aggregate(f.dyd, dy, sum)

    f.dyn <- formula(paste0(N_col, "~", long_col, "+", lat_col, "+", age_col))
    dyn <- aggregate(f.dyn, dy, sum)
    dya <- merge(dya, dyn)

    ## merge on data_type
    dyu <- unique(dy[, c(long_col, lat_col, dt_col)])
    dya <- merge(dya, dyu)

    ## now, for each unique lat-long, calculate 5q0
    ll <- unique(cbind(dya[[long_col]], dya[[lat_col]]))
    colnames(ll) <- c(long_col, lat_col)
    fq0 <- numeric(dim(ll)[1])
    for (i in 1:length(fq0)) {

      ## get the part of the dataset at this location
      llr <- which(dya[[long_col]] == ll[i, 1] & dya[[lat_col]] == ll[i, 2])
      dll <- dya[llr, ]
      ages <- dll[[age_col]]

      ## get all the probabilities at this location
      if (1 %in% ages) {
        r1 <- which(ages == 1)
        p1 <- sum(dll[[died_col]][r1]) / sum(dll[[N_col]][r1])
      } else {
        p1 <- 0
      }
      if (2 %in% ages) {
        r2 <- which(ages == 2)
        p2 <- sum(dll[[died_col]][r2]) / sum(dll[[N_col]][r2])
      } else {
        p2 <- 0
      }
      if (3 %in% ages) {
        r3 <- which(ages == 3)
        p3 <- sum(dll[[died_col]][r3]) / sum(dll[[N_col]][r3])
      } else {
        p3 <- 0
      }
      if (4 %in% ages) {
        r4 <- which(ages == 4)
        p4 <- sum(dll[[died_col]][r4]) / sum(dll[[N_col]][r4])
      } else {
        p4 <- 0
      }

      ## calculate 5q0 at this location
      fq0[i] <- 1 - (1 - p1)^1 * (1 - p2)^11 * (1 - p3)^24 * (1 - p4)^24
      ## store datatype
      ##      data.t <- dll[[dt_col]]
      ##      if(length(unique(data.t)) > 1){
      ##        message(paste0("Different wts on row: ", i))
      ##      }else{
      ##        if(unique(data.t) == 'cbh') is.cbh[i]  <- 1
      ##        if(unique(data.t) != 'cbh') is.cbh[i]  <- 0
      ## }
    }
    ## summary(fq0) ## just a quick check

    ## store results in a list for either sbh or cbh
    ##      get(paste0("fq0_list_", dt))[[which(yrs == y)]] <- list('loc' = ll,
    ##                                                              'fq0' = fq0)
    ##      assign(paste0("fq0_list_", dt, "_", y), list('loc' = ll,
    ##                                                   'fq0' = fq0)
    ##            )
    fq0_list[[which(yrs %in% y)]] <- list("loc" = ll, "fq0" = fq0)
  }
  #  }

  ## add year names to list
  names(fq0_list_sbh) <- names(fq0_list_cbh) <- yrs
  names(fq0_list) <- yrs

  ## save pdfs for each year

  ## load africa country shapefile
  as <- shapefile("/home/j/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad0.shp")

  ## ## find the range of the data and use that to make sure all legends
  ## ## are on the same scale by making up a few datapoints outside the
  ## ## scope of the plot. don't need this since it goes 0 to 1
  ## for(i in 1:length(fq0_list)){
  ##   fq0.r <- range(fq0_list[[i]][["fq0"]])
  ##   if(i == 1) r <- fq0.r
  ##   if(fq0.r[1] < r[1]) r[1] <- fq0.r[1]
  ##   if(fq0.r[2] > r[2]) r[2] <- fq0.r[2]
  ##   ##  message(fq0.r)
  ##   ##  message(paste0("on", i))
  ##   ##  message(sum(fq0_list[[i]][['fq0']] == 1))
  ##   ##  message(length(fq0_list[[i]][['fq0']]))
  ## }
  ## ## for(i in 1:4){
  ## ##   fq0_list[[i]][['loc']] <- rbind(fq0_list[[i]][['loc']], matrix(rep(10, 8), ncol = 2)) ## add locations in nigeria
  ## ##   fq0_list[[i]][['fq0']] <- c(fq0_list[[i]][['fq0']], fq0.r, fq0.r) ## range
  ## ##   fq0_list[[i]][['cbh']] <- c(fq0_list[[i]][['cbh']], 1, 1, 0, 0)   ## both cbh and sbh data
  ## ## }


  ## set the colors
  breaks <- c(
    0, 25,
    26:50,
    51:200,
    1000
  )
  col.f1 <- colorRampPalette(c("#e58bba", "#f2e8b5"))
  col.f2 <- colorRampPalette(c("#f2e8b5", "#ed152e"))
  col <- c(
    "#74039E",
    col.f1(25),
    col.f2(150),
    "#ED152E"
  )


  for (i in 1:4) {
    pdf(paste0(save_dir, "/raw_fq0_quilt_all", names(fq0_list)[i], ".pdf"), height = dim, width = dim)
    par(
      mfrow = c(1, 1),
      mar = c(5, 4, 4, 4)
    )
    plot(as, main = "")
    quilt.plot(fq0_list[[i]][["loc"]][, 1],
      fq0_list[[i]][["loc"]][, 2],
      fq0_list[[i]][["fq0"]] * 1000,
      main = "CBH",
      add = T,
      breaks = breaks, col = col,
      FUN = median
    )
    plot(as, add = T)

    ## plot(as,  main = "SBH",)
    ## quilt.plot(fq0_list_sbh[[i]][['loc']][, 1],
    ##            fq0_list_sbh[[i]][['loc']][, 2],
    ##            fq0_list_sbh[[i]][['fq0']] * 1000,
    ##            main = "SBH",
    ##            add = T,
    ##            breaks = breaks, col = col,
    ##            FUN = median)
    ## plot(as, add = T)

    dev.off()
  }



  if (FALSE) {

    ## akima::interp() attempt

    save(list = ls(), file = "~/plot.RData")
    load("~/plot.RData")

    rows <- base:::sample(size = 2e4, 1:length(fq0))
    ##  rows <- 1:length(fq0)

    system.time(fit <- interp(ll[rows, 1], ll[rows, 2], fq0[rows]))

    pdf("~/interp.pdf", width = 10, height = 10)
    image(fit)
    contour(fit, add = TRUE)
    points(ll[rows, ], pch = 3)
    dev.off()
  }

  if (FALSE) {
    ## Thin plate spline attempt

    ## now we make the thin-plate smoothed version
    dircos <- function(x1) {
      coslat1 <- cos((x1[, 2] * pi) / 180)
      sinlat1 <- sin((x1[, 2] * pi) / 180)
      coslon1 <- cos((x1[, 1] * pi) / 180)
      sinlon1 <- sin((x1[, 1] * pi) / 180)
      cbind(coslon1 * coslat1, sinlon1 * coslat1, sinlat1)
    }

    tps.name <- paste0("tps.fit.", y)
    system.time( ## this is slow
      tps.fit <- Tps(dircos(ll), fq0)
    )
    assign(tps.name, tps.fit)

    ## project where we want to get estimates
    ## preds <- predict(tps.fit, x=as.matrix(proj.ll))

    ## now we just need to store these back in the raster correctly (or make a new raster)
  }

  print("Done making images")
  message("Done making images")
}
