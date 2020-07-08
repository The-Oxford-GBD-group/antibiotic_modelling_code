#' @title Make AROC
#' @description Generates a set of aroc objects from cell_preds or admin_preds, for use in
#' projections or in other analyses that require AROC
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_\%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_\%s_0.RData', reg)
#' @param type Type of aroc to create. Options include \code{cell}, \code{admin},
#'   or \code{c('cell', 'admin')} for both.
#' @param measure prevalence, incidence, mortality, etc
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param raked Should we do this with raked cell/admin preds?
#' @param weighting_res Spatial resolution to apply weights. Can be done by either:
#'    'domain' (e.g. Africa),
#'    'country'.
#'    In the future this could be expanded to adm* levels. Currently this only affects
#'    weighting if you also select weighting_type==empirical
#' @param weighting_type Method to generate the weighting used in the weighted-mean of the
#'    rate of changes calculated between all adjacent pairs of years in year_list.
#'    Can be: 'exponential' (see pow argument for details) or 'empirical'.
#'    Empirical generates weights proportional to the amount of data in each year.
#'      - The total sample size across all years divided by num_years is added to every year
#'        to ensure no pairs of years get zero weight
#'      - If empirical and country are both selected and there are countries with no data then
#'        exponential weights are used
#'      - Must supply input_data
#' @param pow Power used in year-weighting scheme:
#'    exponential weight used in determining year_wt_i:
#'         (yr_i - yr_1)/(yr_n - yr_i))^pow
#'    if pow==0, get uniform wts
#'    if pow==1, get linear wts
#'    if pow > 1, get exponential wts, etc.
#' @param input_data Required to derive emprical weights. A set of cleaned and collapsed micro
#'    data (e.g. input to MBG models) that will be used to derive the emprical weights.
#'    Required Columns:
#'       - country (iso3)
#'       - N
#'       - year
#' @param mult_emp_exp if TRUE and you are calculating emprical weights, the sample-size
#'    driven empirical weights are also multipled by the exponential weighting scheme such
#'    that both the exponential weight and the empirical weight contribute to the final weights
#' @param extra_file_tag Appended at the end of all files generated from this run of the function.
#' Useful if you're comparing different weights/resolutions to calcualte AROC and project
#' @param shapefile_version string specifying which version of shapefiles to pull
#' @return writes cell- and admin- level aroc objects to standard directories and formats
#'   in the 'pred_derivatives' folder of the model run.  cell-level objects are in the
#'   cell_pred indexed format. Both cell- and admin- aroc objects are matrices wide by draw.
#' NOTE: admin_preds are sorted such that the order of the rows is by year and then admin_code
#' @examples
#' \dontrun{
#' make_aroc(
#'   ind_gp = indicator_group,
#'   ind = indicator,
#'   rd = run_date,
#'   matrix_pred_name = NULL,
#'   type = c("cell", "admin"),
#'   measure = "prevalence",
#'   year_list = c(2000:2015),
#'   uselogit = TRUE,
#'   raked = TRUE,
#'   weighting_res = "country",
#'   weighting_type = "empirical",
#'   pow = 1,
#'   input_data = clean_collapsed_micro_data,
#'   mult_emp_exp = TRUE
#' )
#' }
#' 
#' @rdname make_aroc
#'
#' @export
make_aroc <- function(ind_gp, ind, rd,
                      matrix_pred_name = NULL,
                      type,
                      measure = "mortality",
                      skip_cols = NULL,
                      year_list = c(2000:2015),
                      uselogit = FALSE,
                      raked,
                      weighting_res = "domain",
                      weighting_type = "exponential",
                      pow = 1,
                      input_data = NULL,
                      mult_emp_exp = FALSE,
                      extra_file_tag = "",
                      shapefile_version = "current") {
  if (weighting_type == "emprical" & is.null(input_data)) {
    warning("You must supply input_data if using 'emprical' weighting.")
    stop()
  }

  # define directories
  share_dir <- paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/")
  output_dir <- paste0(share_dir, "/pred_derivatives/aroc/")
  dir.create(output_dir, showWarnings = F, recursive = T)

  ## load in regions used in this model and run_date
  regions <- get_output_regions(in_dir = share_dir)

  ## get weights to use in either cell or adm aroc calculations
  aroc_weights <- make_aroc_weights(
    weighting_res = weighting_res,
    weighting_type = weighting_type,
    pow = pow,
    year_list = year_list,
    input_data = input_data,
    mult_emp_exp = mult_emp_exp
  )

  ## we also get all the gaul codes that came back from
  ## make_arox_weights() if we're using country resolution
  if (weighting_res == "country") {
    aroc_weight_gauls <- gaul_convert(aroc_weights$country, from = "iso3", shapefile_version = shapefile_version)
  }

  ## we also get the exponential weights which we may need in the
  ## empirical setting if there's no data in a predicted country
  exp_weights <- make_aroc_weights(
    weighting_res = "domain",
    weighting_type = "exponential",
    pow = pow,
    year_list = year_list,
    input_data = NULL,
    mult_emp_exp = FALSE
  )

  ## get number of years
  num_yrs <- length(year_list)

  if ("cell" %in% type) {
    message("Working on CELL level")
    for (ii in 1:length(regions)) {
      message(sprintf("- On region: %s", regions[ii]))

      cell_pred <- get_cell_pred_for_aroc(ind_gp,
        ind,
        rd,
        regions[ii],
        measure,
        matrix_pred_name,
        skip_cols,
        rk = raked,
        shapefile_version = shapefile_version
      )

      if (weighting_res == "country") {
        ## we'll need to match pixels to countries so we load the region simple raster
        message("-- making simple raster")
        gaul_list <- get_adm0_codes(regions[ii], shapefile_version = shapefile_version)
        simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 0.4, subset_only = T)
        subset_shape <- simple_polygon_list[[1]]
        raster_list <- build_simple_raster_pop(subset_shape)
        simple_raster <- raster_list[["simple_raster"]] ## this is what we really need

        ## pull out gaul codes and match to cell preds via cell_idx
        cell_idx <- notMissingIdx(simple_raster)
        ctry_vec <- values(simple_raster)[cell_idx]
        all_ctrys <- sort(unique(ctry_vec))
      }

      ## now we have some year column and some id column and the rest are draw columns.
      ## we can now calculate AROC by idx across time
      num_draws <- ncol(cell_pred) - 2
      num_idx <- length(unique(cell_pred[, 2])) ## second col is 'idx'

      ## for each draw, calculate the rates of change between years and
      ## then the weighted total AROC across all years
      message("-- making AROC between years for each draw")
      aroc_draws <- matrix(ncol = num_draws, nrow = num_idx)
      # goal_draws <- copy(aroc_draws)
      if (uselogit) {
        cell_pred_logit <- cell_pred
        cell_pred_logit[, 3:ncol(cell_pred)] <- log(cell_pred[, 3:ncol(cell_pred)] / (1 - cell_pred[, 3:ncol(cell_pred)]))
      }
      for (dd in 1:(ncol(cell_pred) - 2)) {
        if (dd %% 50 == 1) message(sprintf("---- on draw %i out of %i", dd, num_draws))
        aroc_mat <- matrix(ncol = num_yrs - 1, nrow = num_idx)
        for (yy in 1:(num_yrs - 1)) {
          if (uselogit) {
            aroc_mat[, yy] <- cell_pred_logit[1:num_idx + (yy) * num_idx, dd + 2] - cell_pred_logit[1:num_idx + (yy - 1) * num_idx, dd + 2]
          } else {
            aroc_mat[, yy] <- log(cell_pred[1:num_idx + (yy) * num_idx, dd + 2]) - log(cell_pred[1:num_idx + (yy - 1) * num_idx, dd + 2])
          }
        }

        if (weighting_res == "domain") {
          ## then there is only a single weight vector for all locs
          aroc_vec <- aroc_mat %*% aroc_weights
        }
        if (weighting_res == "country") {
          ## we have a different set of weights for each country and must match accordingly
          aroc_vec <- rep(0.0, nrow(aroc_mat))
          for (cc in 1:length(all_ctrys)) {
            ctry_gaul <- all_ctrys[cc]
            ctry_idx <- which(ctry_vec == ctry_gaul)
            if (ctry_gaul %in% aroc_weight_gauls) { ## use empirical weight if possible
              ctry_wt <- as.numeric(aroc_weights[which(aroc_weight_gauls == ctry_gaul), 2:ncol(aroc_weights)])
            } else { ## otherwise use exponential weight
              ctry_wt <- exp_weights
            }
            aroc_vec[ctry_idx] <- aroc_mat[ctry_idx, ] %*% as.vector(ctry_wt)
          }
        }

        aroc_draws[, dd] <- aroc_vec
        # goal_draws[, dd] <- ifelse(aroc_vec < aroc_goal, 1, 0)
      }
      # short_goal <- ifelse(aroc_draws < aroc_goal_2015, 1, 0)
      # goal_draws[is.na(goal_draws)] <- 0
      # short_goal[is.na(short_goal)] <- 0
      # relative_goal_prob <- rowMeans(goal_draws)
      # achieved_relative_prob <- rowMeans(short_goal)
      message(sprintf("TESTING: Percent of NA rows per column is: %f%%", mean(is.na(aroc_draws[, 1]))))
      message("-- finished making AROC across draws. now saving")
      saveRDS(
        object = aroc_draws,
        file = sprintf(
          "%s/%s_%s_aroc_cell_draw_matrix_%s%s%s.RDs",
          output_dir, ind, measure, ifelse(uselogit, "logit_", ""), regions[ii],
          extra_file_tag
        )
      )
    } # Close region loop
  } # if ('cell' %in% type)

  if ("admin" %in% type) {
    message("Working on ADMIN level")
    ## load the admin objects
    ## try two different locations until we standardize
    file_1 <- sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/%s_%s_admin_draws_",
      ifelse(raked, "raked", "unraked"), ".Rdata",
      ind_gp, ind, rd, ind, measure
    )
    file_2 <- paste0(
      "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/",
      ind, "_", ifelse(raked, "raked", "unraked"),
      "_admin_draws_eb_bin0_0.RData"
    )

    if (file.exists(file_1)) {
      load(file_1)
    } else if (file.exists(file_2)) {
      load(file_2)
    } else {
      stop("Cannot load admin pred object!")
    }

    ## load spatial admin hierarchy
    admins <- get_sp_hierarchy(shapefile_version = shapefile_version)

    ## this contains admin_0, admin_1, and admin_2 matrix_draw objects
    ## col1 is year, col2 is ADM*_CODE, final column is pop. all other columns are draws

    for (aa in 0:2) { ## for each admin type
      message(sprintf("- On admin: %i", aa))

      cell_pred <- get(sprintf("admin_%i", aa))
      cell_pred <- as.data.table(cell_pred)

      ## format this object to look like the one we used for cell level (easier to copy code from above)
      ## revised now to work with a bigger array of cell pred objects
      str_match <- stringr::str_match
      draw_cols <- names(cell_pred)[grep("V[0-9]*", names(cell_pred))]
      keep_cols <- c("year", paste0("ADM", aa, "_CODE"), draw_cols)
      cell_pred <- subset(cell_pred, select = keep_cols)
      setnames(cell_pred, paste0("ADM", aa, "_CODE"), "idx")
      cell_pred <- as.matrix(cell_pred)

      ## it needs to be ordered like the regular cell_pred object too!
      ## all admin_idx for a single year show up, then repeat
      cell_pred <- cell_pred[order(cell_pred[, 1], cell_pred[, 2]), ]

      num_draws <- ncol(cell_pred) - 2

      # Redid the below line to warn if mismatches in case of non-unique admin codes - JM
      num_idx <- nrow(cell_pred) / length(year_list)
      if (num_idx != length(unique(cell_pred[, 2]))) {
        warning(paste0(
          "The number of unique cell_pred indices does not equal the number of ",
          "rows of cell_pred divided by the number of years.",
          "\nYou may have duplicate admin codes at the admin ", aa,
          " level - check your aggregated objects!"
        ))
      }
      num_yrs <- length(unique(cell_pred[, 1]))
      idx <- cell_pred[which(cell_pred[, 1] == min(cell_pred[, 1])), 2] ## idx in year1

      if (weighting_res == "country") {
        if (aa == 0) {
          ctry_vec <- idx
        } else {
          sp_aa <- admins[[sprintf("ADM%i", aa)]]
          aa_vec <- cell_pred[, sprintf("ADM%i_CODE", aa), with = F]
          aa_vec[, ind := 1:nrow(aa_vec)]
          ctry_vec <- merge(aa_vec, sp_aa, all.y = FALSE)
          ctry_vec <- ctry_vec[order(ind), ADM0_CODE]
          ctry_vec <- ctry_vec[1:num_idx]
        }
        ## get all the countries in the pred object
        all_ctrys <- sort(unique(ctry_vec))
      }

      ## for each draw, calculate the rates of change between years and
      ## then the weighted total AROC across all years
      message("-- making AROC between years for each draw")
      aroc_draws <- matrix(ncol = num_draws, nrow = num_idx)

      if (uselogit) {
        cell_pred_logit <- cell_pred
        cell_pred_logit[, 3:ncol(cell_pred)] <- log(cell_pred[, 3:ncol(cell_pred)] / (1 - cell_pred[, 3:ncol(cell_pred)]))
      }

      for (dd in 1:(ncol(cell_pred) - 2)) {
        if (dd %% 50 == 1) message(sprintf("---- on draw %i out of %i", dd, num_draws))
        aroc_mat <- matrix(ncol = num_yrs - 1, nrow = num_idx)
        for (yy in 1:(num_yrs - 1)) {
          if (uselogit) {
            aroc_mat[, yy] <- cell_pred_logit[1:num_idx + (yy) * num_idx, dd + 2] - cell_pred_logit[1:num_idx + (yy - 1) * num_idx, dd + 2]
          } else {
            aroc_mat[, yy] <- log(cell_pred[1:num_idx + (yy) * num_idx, dd + 2]) - log(cell_pred[1:num_idx + (yy - 1) * num_idx, dd + 2])
          }
        }

        if (weighting_res == "domain") {
          ## then there is only a single weight vector for all locs
          aroc_vec <- aroc_mat %*% aroc_weights
        }
        if (weighting_res == "country") {
          ## we have a different set of weights for each country and must match accordingly
          aroc_vec <- rep(0.0, nrow(aroc_mat))
          for (cc in 1:length(all_ctrys)) {
            ctry_gaul <- all_ctrys[cc]
            ctry_idx <- which(ctry_vec == ctry_gaul)
            if (ctry_gaul %in% aroc_weight_gauls) { ## use empirical weight if possible
              ctry_wt <- as.numeric(aroc_weights[which(aroc_weight_gauls == ctry_gaul), 2:ncol(aroc_weights)])
            } else { ## otherwise use exponential weight
              ctry_wt <- exp_weights
            }
            aroc_vec[ctry_idx] <- aroc_mat[ctry_idx, ] %*% as.vector(ctry_wt)
          }
        }

        aroc_draws[, dd] <- aroc_vec
      }

      message(sprintf("TESTING: Percent of NA rows per column is: %f%%", mean(is.na(aroc_draws[, 1]))))
      message("-- finished making AROC across draws. now saving")
      final_aroc <- cbind(idx, aroc_draws)
      colnames(final_aroc)[1] <- sprintf("ADM%i_CODE", aa)

      saveRDS(
        object = final_aroc,
        file = sprintf(
          "%s/%s_%s_aroc_adm%i_draw_matrix%s%s.RDs",
          output_dir, ind, measure, aa, ifelse(uselogit, "_logit", ""),
          extra_file_tag
        )
      )
    } # For aa in admin
  } # if ('admin' %in% type)...
}
