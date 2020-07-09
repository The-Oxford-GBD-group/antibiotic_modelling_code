#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param regions PARAM_DESCRIPTION, Default: strata
#' @param summstats PARAM_DESCRIPTION, Default: c("mean", "cirange", "upper", "lower")
#' @param raked PARAM_DESCRIPTION, Default: c("raked", "unraked")
#' @param proj Look at projection files? Default: FALSE
#' @param rf_table PARAM_DESCRIPTION, Default: TRUE
#' @param run_summ PARAM_DESCRIPTION, Default: TRUE
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param ig PARAM_DESCRIPTION, Default: indicator_group
#' @param sdir PARAM_DESCRIPTION, Default: sharedir
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{merge}}
#' @rdname post_load_combine_save
#' @export
#' @importFrom raster merge
post_load_combine_save <- function(regions = strata,
                                   summstats = c("mean", "cirange", "upper", "lower"),
                                   raked = c("raked", "unraked"),
                                   rf_table = TRUE,
                                   run_summ = TRUE,
                                   indic = indicator,
                                   ig = indicator_group,
                                   sdir = sharedir,
                                   proj = FALSE,
                                   proj_folder = NULL) {
  message(paste0("indic: ", indic))
  message(paste0("ig: ", ig))

  rake_addin <- character()
  if ("unraked" %in% raked) {
    lookup_dir <- paste0(sprintf("share/geospatial/mbg/%s/%s/output/%s/", ig, indic, run_date))
    ur <- length(grep(paste0(indic, ".*unraked.*raster.tif"), list.files(lookup_dir)))
    if (proj) ur <- length(grep(paste0(indic, ".*unraked_PROJ.*raster_PROJ.tif"), list.files(lookup_dir)))
    if (ur > 0) rake_addin <- c(rake_addin, unraked = "_unraked")
    if (ur == 0) rake_addin <- c(rake_addin, unraked = "")
  }

  if ("raked" %in% raked) {
    rake_addin <- c(rake_addin, raked = "_raked")
  }

  # loop through and combine all rasters
  message("\nCombining rasters...")
  for (rake in rake_addin) {
    message(names(rake_addin)[which(rake_addin == rake)])
    rr <- rake
    for (ss in summstats) {
      message(paste0("  ", ss))
      rlist <- list()
      for (reg in regions) {
        message(paste0("    ", reg))
        rlist[[reg]] <-
          brick(ifelse(proj,
            sprintf("/share/geospatial/mbg/%s/%s/output/%s/%s_%s%s_%s_raster_PROJ.tif", ig, indic, run_date, indic, reg, rake, ss),
            sprintf("/share/geospatial/mbg/%s/%s/output/%s/%s_%s%s_%s_raster.tif", ig, indic, run_date, indic, reg, rake, ss)
          ))
      }
      if (length(rlist) > 1) rlist <- do.call(raster::merge, unname(rlist)) else rlist <- rlist[[1]]
      if (ss == "cirange") ssname <- "range" else ssname <- ss # naming convention
      save_post_est(
        rlist, "raster",
        ifelse(!proj,
          paste0(ssname, rr, "_raster"),
          paste0(ssname, rr, "_raster_PROJ")
        ),
        indic
      )
    }
  }

  # do rf also
  if (rf_table) {
    message("RF table")
    rflist <- list()
    for (reg in regions) {
      rflist[[reg]] <-
        if (proj) {
          read.csv(sprintf("/share/geospatial/mbg/%s/%s/output/%s/%s_%s_rf_PROJ.csv", ig, indic, run_date, indic, reg))
        } else {
          read.csv(sprintf("/share/geospatial/mbg/%s/%s/output/%s/%s_%s_rf.csv", ig, indic, run_date, indic, reg))
        }
    }
    if (!proj) {
      save_post_est(do.call(rbind.fill, rflist), "csv", "rf", indic)
    } else {
      save_post_est(do.call(rbind.fill, rflist), "csv", "rf_PROJ", indic)
    }
  }

  # make a run summary graph
  if (run_summ) {
    graph_run_summary(
      run_date = run_date,
      indicator_group = ig,
      indicator = indic
    )
  }
}
