#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ct PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param n_folds PARAM_DESCRIPTION, Default: 5
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ct_folds
#' @export
ct_folds <- function(xy, ## xy location matrix
                     ct, ## country vec
                     ss = 1, ## sample size vec (or 1 if equi-ss)
                     n_folds = 5,
                     ...) {



  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (4) countries in space
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  ## ## example
  ## df <- read.csv('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',
  ##                stringsAsFactors = FALSE)
  ## shp_full <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')

  ## df$long <- as.numeric(as.character(gsub(",", "", df$long)))
  ## df$lat  <- as.numeric(as.character(gsub(",", "", df$lat)))
  ## df$country <- gsub(pattern='Guinea-Bissau\\"',replacement="Guinea-Bissau", df$country)
  ## df <- df[-which(df$lat>90), ]
  ## data <- df

  ## xy <- data[,c('long', 'lat')]
  ## ss <- data$exposed
  ## ct <- data$country
  ## folds <- ct_folds(xy, ct, ss)
  ## plot(shp_full)
  ## library(scales)
  ## points(xy, col=alpha(folds, alpha=0.25), pch=".")

  if (length(unique(yr) < n_folds)) {
    message("Too many folds for too few countries! Expand your horizons")
    stop()
  }

  if (length(ss) == 1) ss <- rep(1, nrow(xy))

  ## first we find the sample size in each of the countries
  dt <- data.table(
    long = xy[, 1],
    lat = xy[, 2],
    ss = ss,
    ct = ct
  )

  ## get sample size totals in each country
  cts_in_ct <- dt[, sum(ss), by = ct]

  ## make the folds
  fold_vec <- make_folds_by_poly(
    cts_in_polys = as.data.frame(cts_in_ct),
    pt_poly_map = as.character(dt[, ct]),
    n_folds = n_folds
  )


  return(fold_vec)
}
