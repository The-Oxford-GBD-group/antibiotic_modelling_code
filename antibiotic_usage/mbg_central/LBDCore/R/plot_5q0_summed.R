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
#' @rdname plot_5q0_summed
#' @export
plot_5q0_summed <- function(data = df,
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


  #######################################################################################
  ## this one does a coarser summed estimate
  ## it sums exp in a box, sums deaths in a box and maps 1-(1-sum(death)/sum(exp))^60
  ## WARNING: still in development though all the code needed to produce maps is located internally


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

  ## set the colors
  breaks <- c(
    0, 25,
    26:50,
    51:200,
    202
  )
  col.f1 <- colorRampPalette(c("#e58bba", "#f2e8b5"))
  col.f2 <- colorRampPalette(c("#f2e8b5", "#ed152e"))
  col <- c(
    "#74039E",
    col.f1(25),
    col.f2(150),
    "#ED152E"
  )


  ## load africa country shapefile
  as <- shapefile("/home/j/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad0.shp")

  for (y in yrs) {
    print(paste0("On year: ", y))

    pdf(paste0(save_dir, "/raw_fq0_new_all", y, ".pdf"), height = dim, width = dim)
    par(
      mfrow = c(1, 1),
      mar = c(5, 4, 4, 5)
    )

    #   for(dt in c("cbh", "sbh")){
    #    print(paste0("~~~ ON DATA TYPE: ", dt))

    dy <- d[which(d[[yr_col]] == y), ] # & d[[dt_col]] == dt), ] ## get the subset of the data for the year and data type

    ## ## aggregate died_col by lat-long location and combine aggregates into one frame
    ## f.dyd <- formula(paste0(died_col, "~", long_col, "+", lat_col, "+", age_col))
    ## dya <- aggregate(f.dyd, dy, sum)

    ## f.dyn <- formula(paste0(N_col, "~", long_col, "+", lat_col, "+", age_col))
    ## dyn <- aggregate(f.dyn, dy, sum)
    ## dya <- merge(dya, dyn)

    ## ## merge on data_type
    ## dyu <- unique(dy[, c(long_col, lat_col, dt_col)])
    ## dya <- merge(dya, dyu)

    ## get the prob of dying in the age bin from the sums in the square
    ## first, make a grid that will be constant for y and dt
    gr.y.dt <- discretize.image(x = dy[, 1:2], m = 64, n = 64, grid = NULL, boundary.grid = FALSE)$grid

    for (a in 1:4) {
      ## subset the data
      dya <- subset(dy, get(age_col) == a)

      ## now we use the hacked quilt.plot to sum on grid squares
      sum.deaths <- as.image(
        Z = dya$died, x = dya[, 1:2], nx = 64, ny = 64, na.rm = TRUE,
        grid = gr.y.dt, FUN = sum
      )
      sum.exps <- as.image(
        Z = dya$N, x = dya[, 1:2], nx = 64, ny = 64, na.rm = TRUE,
        grid = gr.y.dt, FUN = sum
      )

      sum.ratio <- sum.deaths$z / sum.exps$z
      not.na.z <- which(!is.na(sum.ratio))

      if (a == 1) {
        dyp <- cbind(
          not.na.z,
          sum.ratio[not.na.z]
        )
        colnames(dyp) <- c("n.na.z", paste0("s.r", a))
      } else {
        dy.t <- cbind(
          not.na.z,
          sum.ratio[not.na.z]
        )
        colnames(dy.t) <- c("n.na.z", paste0("s.r", a))
        dyp <- merge(dyp, dy.t, all = T, ny = "n.na.z")
      }
    }

    ## fill in NAs with zeros
    dyp <- as.matrix(dyp)
    dyp[which(is.na(dyp))] <- 0

    ## make 5q0 estimates
    fq0 <- 1 - (1 - dyp[, 2])^1 * (1 - dyp[, 3])^11 * (1 - dyp[, 4])^24 * (1 - dyp[, 5])^24

    ## mask values higher than 200 to 201 for plotting purposes
    ## the color scale stops at 200 anyways
    fq0[which(fq0 > 200)] <- 201


    ## make '5q0' estimate from the summed guys
    ##    fq0.z <- 1 - (1 - sum.deaths$z/sum.exps$z) ^ 60


    ## set up quilt.plot
    plot.im <- as.image(
      Z = dy$died, x = dy[, 1:2], nx = 64, ny = 64, na.rm = TRUE,
      grid = gr.y.dt, FUN = sum
    )

    ## fill z with our numbers
    z <- plot.im$z
    z[dyp[, 1]] <- fq0
    plot.im$z <- z * 1000 ## convert to counts

    ## now we can plot it
    plot(as, main = "All")
    image.plot(plot.im, nlevel = length(col), col = col, add = TRUE, breaks = breaks)
    plot(as, add = TRUE)

    dev.off()
  }

  #    dev.off()
  #  }

  ## add year names to list
  names(fq0_list_sbh) <- names(fq0_list_cbh) <- yrs

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
    pdf(paste0(save_dir, "/raw_fq0_quilt_cbh_sbh", names(fq0_list_cbh)[i], ".pdf"), height = dim, width = dim * 2)
    par(
      mfrow = c(1, 2),
      mar = c(5, 4, 4, 10)
    )
    plot(as, main = "CBH")
    quilt.plot(fq0_list_cbh[[i]][["loc"]][, 1],
      fq0_list_cbh[[i]][["loc"]][, 2],
      fq0_list_cbh[[i]][["fq0"]] * 1000,
      main = "CBH",
      add = T,
      breaks = breaks, col = col,
      FUN = median
    )
    plot(as, add = T)

    plot(as, main = "SBH")
    quilt.plot(fq0_list_sbh[[i]][["loc"]][, 1],
      fq0_list_sbh[[i]][["loc"]][, 2],
      fq0_list_sbh[[i]][["fq0"]] * 1000,
      main = "SBH",
      add = T,
      breaks = breaks, col = col,
      FUN = median
    )
    plot(as, add = T)

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
