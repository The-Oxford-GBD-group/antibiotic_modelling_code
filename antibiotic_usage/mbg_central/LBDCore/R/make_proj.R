#' @title Make projection
#' @description Generates a set of draw-level projection objects from aroc objects for
#' a given set of target years
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param type Type of aroc to create. Options include \code{cell}, \code{admin},
#'   or \code{c('cell', 'admin')} for both.
#' @param proj_years Vector (integer) of years that you want to project to.  Note
#'   that this is different from \code{year_list}, which is the list of years that
#'   were included in the model run / are included in the aroc object.
#' @param measure prevalence, incidence, mortality, etc
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param raked Should we do this with raked cell/admin preds?
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_\%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_\%s_0.RData', reg)
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param extra_file_tag Appended at the end of all files generated from this run of the function. Useful if you're comparing different weights/resolutions to calcualte AROC and project
#' @return writes cell- and admin- level projection objects to standard directories and formats
#'   in the 'pred_derivatives' folder of the model run.  cell-level objects are in the
#'   cell_pred indexed format. Both cell- and admin- projection objects are matrices wide by draw.
#' NOTE: admin_preds are sorted such that the order of the rows is by year and then admin_code
#' @examples
#' \dontrun{
#' make_proj(
#'   ind_gp = indicator_group,
#'   ind = indicator,
#'   rd = run_date,
#'   type = c("cell", "admin"),
#'   proj_years = c(2020, 2025, 2030),
#'   measure = "prevalence",
#'   skip_cols = NULL,
#'   year_list = c(2000:2015),
#'   uselogit = TRUE
#' )
#' }
#' @export
make_proj <- function(ind_gp, ind, rd,
                      type,
                      proj_years = c(2020, 2025),
                      measure = "mortality",
                      skip_cols = NULL,
                      raked,
                      matrix_pred_name = NULL,
                      year_list = c(2000:2015),
                      uselogit = FALSE,
                      extra_file_tag = "",
                      shapefile_version = "current") {

  ## load in regions used in this model and run_date
  regions <- get_output_regions(in_dir = paste0(
    "/share/geospatial/mbg/",
    ind_gp, "/",
    ind, "/output/",
    rd
  ))

  # define directories
  share_dir <- paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/")
  aroc_dir <- paste0(share_dir, "/pred_derivatives/aroc/")
  output_dir <- paste0(share_dir, "/pred_derivatives/proj/")

  dir.create(output_dir, recursive = T, showWarnings = F)

  ## make projections at the cell level
  if ("cell" %in% type) {
    message("Working on CELL level")
    for (ii in 1:length(regions)) {
      message(sprintf("- On region: %s", regions[ii]))

      # Pull cell pred
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

      # Load aroc object
      message("-- loading aroc")

      aroc_draws <- readRDS(sprintf(
        "%s/%s_%s_aroc_cell_draw_matrix_%s%s%s.RDs",
        aroc_dir, ind, measure, ifelse(uselogit, "logit_", ""),
        regions[ii], extra_file_tag
      ))

      # Get number of draws
      num_draws <- ncol(aroc_draws)
      if (ncol(aroc_draws) != ncol(cell_pred) - 2) stop("cell_pred & aroc draw #s do not match!")

      # Make projections -----------------------------------
      ## also make projections and save them
      message("-- making projections")

      ## grab last year of modeled estiamtes
      final_year <- cell_pred[which(cell_pred[, 1] == max(cell_pred[, 1])), ] ## first col is year
      last_year <- final_year[, -(1:2)]

      ## grab idx
      idx <- final_year[, 2]

      ## unlist all draws into vector, apply forecast, and convert back to matrix
      final_year <- as.vector(final_year[, -(1:2)]) ## unlist only the draw columns

      ## unlist aroc draw matrix
      aroc_draws <- as.vector(aroc_draws)

      ## set up a list to capture the projection output
      proj_draws_list <- list()

      for (yr in proj_years) {
        message(paste0("--- year: ", yr))

        ## figure out how many years to project
        proj_dur <- as.numeric(yr) - max(year_list)

        ## make projection from final_yr out proj_dur years
        if (uselogit) {
          proj_draws_logit <- log(final_year / (1 - final_year)) + (aroc_draws * proj_dur)
          proj_draws <- exp(proj_draws_logit) / (1 + exp(proj_draws_logit))
        } else {
          proj_draws <- final_year * exp(aroc_draws * proj_dur)
        }

        ## convert back to matrix
        proj_draws <- matrix(proj_draws, ncol = num_draws)

        ## insert into list
        proj_draws_list[[as.character(yr)]] <- proj_draws
        rm(proj_draws)
      }

      ## save
      message("-- saving projections")

      lapply(1:length(proj_draws_list), function(i) {
        saveRDS(
          object = proj_draws_list[[i]],
          file = sprintf(
            "%s/%s_%s_%s_projections_cell_draw_matrix_%s%s.RDs",
            output_dir, ind, measure, names(proj_draws_list)[i],
            ifelse(uselogit, "logit_", ""), regions[ii]
          )
        )
      })
    } # close regions loop
  } # if ('cell' %in% type)

  # make projections as the admin level
  if ("admin" %in% type) {
    message("Working on ADMIN level")
    ## load the admin objects
    ## try two different locations until we standardize
    message("- loading admin objects")
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

    for (aa in 0:2) { ## for each admin type

      message(paste0("- admin level: ", aa))

      # create pseudo cell-pred object
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

      # load aroc
      message("-- loading aroc")
      aroc_draws <- readRDS(sprintf(
        "%s/%s_%s_aroc_adm%i_draw_matrix%s%s.RDs",
        aroc_dir, ind, measure, aa, ifelse(uselogit, "_logit", ""),
        extra_file_tag
      ))

      # remove first column (spatial index) and store separately
      spatial_idx <- aroc_draws[, 1]
      aroc_draws <- aroc_draws[, 2:ncol(aroc_draws)]

      # Get number of draws
      num_draws <- ncol(aroc_draws)
      if (ncol(aroc_draws) != ncol(cell_pred) - 2) stop("cell_pred & aroc draw #s do not match!")

      # make projections
      message("-- making projections")

      ## grab last year of modeled estiamtes
      final_year <- cell_pred[which(cell_pred[, 1] == max(cell_pred[, 1])), ] ## first col is year
      last_year <- final_year[, -(1:2)]

      ## grab idx
      idx <- final_year[, 2]

      ## unlist all draws into vector, apply forecast, and convert back to matrix
      final_year <- as.vector(final_year[, -(1:2)]) ## unlist only the draw columns

      ## unlist aroc draw matrix
      aroc_draws <- as.vector(aroc_draws)

      proj_draws_list <- list()

      ## make projection from final_yr out proj_dur years
      for (yr in proj_years) {
        message(paste0("--- year: ", yr))

        ## figure out how many years to project
        proj_dur <- as.numeric(yr) - max(year_list)

        if (uselogit) {
          proj_draws_logit <- log(final_year / (1 - final_year)) + (aroc_draws * proj_dur)
          proj_draws <- exp(proj_draws_logit) / (1 + exp(proj_draws_logit))
        } else {
          proj_draws <- final_year * exp(aroc_draws * proj_dur)
        }

        ## convert back to matrix
        proj_draws <- matrix(proj_draws, ncol = num_draws)

        ## append spatial index
        proj_draws <- cbind(spatial_idx, proj_draws)

        ## insert into list
        proj_draws_list[[as.character(yr)]] <- proj_draws
        rm(proj_draws)
      }

      ## save
      message("-- saving projections")
      lapply(1:length(proj_draws_list), function(i) {
        saveRDS(
          object = proj_draws_list[[i]],
          file = sprintf(
            "%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs",
            output_dir, ind, measure, names(proj_draws_list)[i],
            aa, ifelse(uselogit, "_logit", "")
          )
        )
      })
    } # close admin levels loop
  } # if ('admin' %in% type)
}
