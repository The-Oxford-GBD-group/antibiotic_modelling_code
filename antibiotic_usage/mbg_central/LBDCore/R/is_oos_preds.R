#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param all.data PARAM_DESCRIPTION, Default: df
#' @param cell_draws_filename PARAM_DESCRIPTION, Default: '\%s_cell_draws_eb_bin\%i_\%s_\%i.RData'
#' @param holdouts PARAM_DESCRIPTION, Default: 5
#' @param reg PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION, Default: \code{2000:2015}
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param indic_group PARAM_DESCRIPTION, Default: indicator_group
#' @param holdoutlist PARAM_DESCRIPTION, Default: NULL
#' @param fun PARAM_DESCRIPTION, Default: 'mean'
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname is_oos_preds
#' @export
#' @importFrom raster extract
is_oos_preds <- function(rd = run_date,
                         all.data = df,
                         cell_draws_filename = "%s_cell_draws_eb_bin%i_%s_%i.RData", ## in sprintf notation
                         holdouts = 5, ## number of holdouts. if zero only does in sample
                         reg,
                         years = 2000:2015,
                         indic = indicator,
                         indic_group = indicator_group,
                         holdoutlist = NULL, ## if null, only does in sample
                         fun = "mean", ## function to sapply over rows
                         shapefile_version = "current",
                         ... ## additional arguments can be passed to `fun`
) {

  ## ################################
  ## is_oos_preds
  ##
  ## this function takes the data that went into our MBG framework and
  ## returns an array containing: location, observed value, in sample
  ## predictions, and out of sample predictions (when applicable), country, year, N
  ##
  ## INPUT:
  ##
  ## OUTPUT:
  ##
  ## #################################


  ## Make data.table
  df <- as.data.table(df)

  ## place to look for things
  output.dir <- sprintf(
    "/share/geospatial/mbg/%s/%s/output/%s/",
    indic_group, indic, rd
  )

  ## Load data
  datdir <- sprintf("/share/geospatial/mbg/%s/%s/output/%s/", indic_group, indic, rd)

  ## holdout data
  if (!is.null(holdoutlist)) {
    d <- data.frame(holdoutlist[[sprintf("region__%s", reg)]])
  } else {
    d <- df
  }

  ## load the simple raster for this region
  message("Loading simple raster...")
  if (file.exists(paste0("/share/geospatial/shapefiles/simple_raster", reg, ".RData"))) {
    load(paste0("/share/geospatial/shapefiles/simple_raster", reg, ".RData"))
  } else {
    simple_polygon_list <- load_simple_polygon(
      gaul_list = get_adm0_codes(regions,
        shapefile_version = shapefile_version
      ),
      buffer = 0.4,
      subset_only = TRUE,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    raster_list <- build_simple_raster_pop(subset_shape)
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]
  }

  ## setup the data in the data order to return
  if (holdouts == 0) {
    return.df <- d
  } else {
    return.df <- d[d$fold == 1, ]
    for (i in 2:holdouts) {
      return.df <- rbind(return.df, d[d$fold == i, ])
    }
  }
  return.df <- return.df[, c(
    "longitude", "latitude", "year", "country", "source",
    indic, "N", "weight"
  ), with = FALSE]

  return.df$OOS <- NA
  return.df$IS <- NA

  ## ####################
  ## get out of sample ##
  ## ####################
  OOS <- NULL

  ## loop through the holdouts
  if (holdouts != 0) {
    message("Starting out-of-sample... ")
    for (hh in 1:holdouts) {

      ## load the associated preds
      message(paste0("Loading cell preds for holdout ", hh, "..."))
      load(sprintf(
        paste0("/share/geospatial/mbg/%s/%s/output/%s/", cell_draws_filename),
        indic_group, indic, rd, indic, 0, reg, hh
      ))

      ## obtain desired summary of cell pred
      message("Summarizing cell preds...")
      ## if mean, will use rowMeans() for speed; otherwise, apply
      if (fun == "mean") {
        ## average across the draws
        summary.cell.pred <- rowMeans(cell_pred)
      } else {
        summary.cell.pred <- apply(cell_pred, 1, get(fun), ...)
      }

      ## turn into a raster
      temp.rast <- insertRaster(simple_raster, matrix(summary.cell.pred, ncol = length(years)))

      ## get the OOS part of the data
      d.oos <- d[d$fold == hh, ]

      ## extract the values at the OOS locations by year
      temp.oos <- numeric(nrow(d.oos))
      for (yr in years) {
        yr.rows <- which(d.oos$year == yr)
        if (length(yr.rows) > 0) {
          temp.oos[yr.rows] <- raster::extract(
            y = cbind(d.oos$longitude, d.oos$latitude)[yr.rows, ],
            x = temp.rast[[which(years == yr)]]
          )
        }
      }

      ## add to OOS vec
      OOS <- c(OOS, temp.oos)
    }

    return.df$OOS <- OOS
  }

  ## ################
  ## get in sample ##
  ## ################
  message("Starting in-sample...")

  ## regardless of whether holdouts==0 or not, we can do in sample extraction
  hh <- 0
  d.is <- d
  d.is$fold <- 0

  ## load the associated preds
  message("Loading cell preds...")
  load(sprintf(
    paste0("/share/geospatial/mbg/%s/%s/output/%s/", cell_draws_filename),
    indic_group, indic, rd, indic, 0, reg, hh
  ))

  ## obtain desired summary of cell pred
  message("Summarizing cell preds...")
  ## if mean, will use rowMeans() for speed; otherwise, apply
  if (fun == "mean") {
    ## average across the draws
    summary.cell.pred <- rowMeans(cell_pred)
  } else {
    summary.cell.pred <- apply(cell_pred, 1, get(fun), ...)
  }

  ## turn into a raster
  temp.rast <- insertRaster(simple_raster, matrix(summary.cell.pred, ncol = length(years)))

  ## extract the values at the OOS locations
  is <- numeric(nrow(d.is))
  for (yr in years) {
    yr.rows <- which(d.is$year == yr)
    if (length(yr.rows) > 0) {
      is[yr.rows] <- raster::extract(
        y = cbind(d.is$longitude, d.is$latitude)[yr.rows, ],
        x = temp.rast[[which(years == yr)]]
      )
    }
  }

  return.df$IS <- is

  ## return data with IS and OOS columns
  return(return.df)
}
