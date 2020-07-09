#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param admin_raster PARAM_DESCRIPTION
#' @param shape_ident PARAM_DESCRIPTION, Default: 'gaul_code'
#' @param admin_shps PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param xy PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param mask_shape PARAM_DESCRIPTION
#' @param mask_raster PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ad2_folds
#' @export
ad2_folds <- function(admin_raster,
                      shape_ident = "gaul_code",
                      admin_shps,
                      ss = 1,
                      xy,
                      n_folds,
                      mask_shape,
                      mask_raster,
                      ...) {

  ## this function takes in admin2 (or any mutually exclusive and
  ## collectively exhaustive) shapefiles covering the domain of your
  ## data and splits your data into folds of approximately equal
  ## sample size using admin2 units to split the data

  ## admin_raster: file location of all pertinent shapefiles to use when folding (.grd)
  ## shape_ident: string identifying data col in shapefile used to uniquely identify polygons
  ## admin_shps: file location of associated raster for admin_raster (.shp)
  ## data: complete dataset that you want to fold
  ## strat_cols: vector of column string names to
  ##    stratify over when making folds. if NULL, holdout
  ##    sets are made across the entire data structure
  ## ss: vector of sample sizes for each row. if <1>, assumes all have ss=1
  ## xy: matrix of xy coordinates
  ## n_folds: number of folds to make
  ## mask_shape: shapefile file location for boundary of area to be folded
  ## mask_raster: raster to project mask_shape onto


  ## ## example
  ## df <- read.csv('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',
  ##                stringsAsFactors = FALSE)

  ## df$long <- as.numeric(as.character(gsub(",", "", df$long)))
  ## df$lat  <- as.numeric(as.character(gsub(",", "", df$lat)))
  ## df <- df[-which(df$lat>90), ]
  ## data <- df

  ## shp_full <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')
  ## folds <- ad2_folds(admin_shps='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad2_raster.grd',
  ##                   shape_ident="gaul_code",
  ##                   admin_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp',
  ##                   data=df,
  ##                   strat_cols=NULL,
  ##                   ss=data$exposed,
  ##                   xy=cbind(data$long, data$lat)
  ##                   n_folds=5,
  ##                   mask_shape='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_simple.shp',
  ##                   mask_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad0_raster')

  ## cols <- folds
  ## cols[which(cols==1)] <- "cyan"
  ## cols[which(cols==2)] <- "red"
  ## cols[which(cols==3)] <- "blue"
  ## cols[which(cols==4)] <- "green"
  ## cols[which(cols==5)] <- "magenta"

  ## png("~/check_folds_plot.png", width=1080, height=1080)
  ## plot(shp_full)
  ## points(df$long, df$lat, col=alpha(cols, alpha=0.01), pch=16)
  ## dev.off()


  ## make a mask for ther region we care about
  mask <- rasterize_check_coverage(shapefile(mask_shape), raster(mask_raster), field = names(shapefile(mask_shape))[1]) * 0

  ## get raster cells in mask
  cell_idx <- cellIdx(mask)

  ## load raster and shapefile for admin units
  rast <- raster(admin_shps)
  rast_cell <- extract(rast, cell_idx)
  shp_full <- shapefile(admin_raster)
  shp <- shp_full@data[c("name", "gaul_code")]
  ## plot(shp_full, col=1:length(shp_full))

  ## match raster polys to gaul
  rast_code <- match(rast_cell, shp$gaul_code)
  rast_code <- shp$gaul_code[rast_code]

  ## get number of datapoints in shapefiles
  shp_cts <- get_sample_counts(
    ss = ss,
    xy = xy,
    shapes = shp_full,
    shape_ident = shape_ident
  )
  pt_shp_map <- shp_cts$pt_poly_map
  cts_in_shps <- shp_cts$ct_mat

  ## make the folds by using the polygons (ad2 units) as the

  ## sampling unit to divide the data into the folds while keeping a
  ## similar sample size in each fold
  fold_vec <- make_folds_by_poly(
    cts_in_polys = cts_in_shps,
    pt_poly_map = pt_shp_map,
    n_folds = n_folds
  )
  ho_id <- pt_shp_map



  return(cbind(
    fold_vec,
    ho_id
  ))
}
