#' @title Compare to target
#' @description Runs comparison of projected years against a set of goals defined in a goal object
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param goal_obj An existing goal object made with \code{add_goal()}
#' @param measure prevalence, incidence, mortality, etc
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param uselogit Should this be done in logit space?
#' @param skip_cols columns to skip when reading in the cell preds
#'   For example, if the first two columns store non-pred information in your
#'   file format, \code{skip_cols = 2} will read in all columns from 3 onwards
#' @param matrix_pred_name In \code{sprintf} notation. The one object passed into
#'   the string should will be a region name. this allows different regions to be
#'   passed to different named matrix_preds (pixel level, ad0, ad1, ad2, ...)
#'   e.g. 'had_diarrhea_cell_draws_eb_bin0_\%s_diarrhea2_0.RData' which
#'   will be passed to sprintf('had_diarrhea_cell_draws_eb_bin0_\%s_0.RData', reg)
#' @param shapefile_version character string indicating version of shapefile to pull
#'
#' @return generates cell- and admin- level probabilities of meeting the specified goal,
#'   according to what is specified in \code{goal_obj}.  Objects are written to
#'   standard directories and formats in the 'pred_derivatives' folder of the model run.
#'   cell-level objects are in the cell_pred indexed format, but are no longer
#'   wide by draw (just a single column as they are probabilities).  admin objects are
#'   saved both as rds files and as .csv files with admin hierarchy appended.
#'
#' @export
compare_to_target <- function(ind_gp,
                              ind,
                              rd,
                              goal_obj,
                              measure,
                              year_list = c(2000:2015),
                              uselogit,
                              raked,
                              skip_cols = NULL,
                              matrix_pred_name = NULL,
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
  proj_dir <- paste0(share_dir, "/pred_derivatives/proj/")
  output_dir <- paste0(share_dir, "/pred_derivatives/target_probs/")

  dir.create(output_dir, showWarnings = F, recursive = T)


  for (n in 1:nrow(goal_obj)) {

    # Load parameters for this row
    target_year <- as.integer(goal_obj[n, target_year])
    target <- as.numeric(goal_obj[n, target])
    target_type <- as.character(goal_obj[n, target_type])
    abs_rel <- as.character(goal_obj[n, abs_rel])
    if (abs_rel == "relative") baseline_year <- as.integer(goal_obj[n, baseline_year])
    proj <- as.logical(goal_obj[n, proj])
    pred_type <- as.character(goal_obj[n, pred_type])
    goal_type <- as.character(goal_obj[n, target_type])
    message(paste0(
      "\n------------------------------------------------",
      "\nWorking on target comparison for:",
      "\n  target_year: ", target_year,
      "\n  target: ", target,
      "\n  target_type: ", target_type,
      "\n  abs_rel: ", abs_rel,
      ifelse(abs_rel == "relative", paste0("\n  baseline_year: ", baseline_year), ""),
      "\n  proj: ", proj,
      "\n  pred_type: ", goal_obj[n, pred_type]
    ), "\n") # character version for pred_type

    # TODO: build in ability to use cell_pred objects too
    if (proj == F) stop("For now, can only use projected values")

    if (grepl("cell", pred_type, fixed = TRUE)) {
      message("Working on CELL level")

      for (ii in 1:length(regions)) {
        message(sprintf("- On region: %s", regions[ii]))

        # Load proj_draws object
        message("-- Loading proj_draws object...")
        proj_draws <- readRDS(sprintf(
          "%s/%s_%s_%s_projections_cell_draw_matrix%s_%s.RDs",
          proj_dir, ind, measure, target_year,
          ifelse(uselogit, "_logit", ""), regions[ii]
        ))

        # Generate probabilities
        if (abs_rel == "absolute") {

          # Calculate cell-wise probability of meeting the goal
          if (target_type == "greater") {
            absolute_goal_draws <- ifelse(proj_draws >= target, 1, 0)
          } else if (target_type == "less") {
            absolute_goal_draws <- ifelse(proj_draws <= target, 1, 0)
          }

          absolute_goal_draws[is.na(absolute_goal_draws)] <- 0
          absolute_goal_prob <- rowMeans(absolute_goal_draws)

          # Save
          message("-- Saving probabilities...")
          saveRDS(
            object = absolute_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
              target_type, "_", target, "_cell_target_probs_", regions[ii], ".RDs"
            )
          )

          rm(absolute_goal_prob, absolute_goal_draws)
        } else if (abs_rel == "relative") {

          # Need to load the pred files for this one...
          # Pull cell pred
          cell_pred <- get_cell_pred_for_aroc(ind_gp,
            ind,
            rd,
            regions[ii],
            measure,
            rk = raked,
            matrix_pred_name,
            skip_cols,
            shapefile_version = shapefile_version
          )

          ## grab baseline year preds
          year_idx <- which(year_list == baseline_year)
          baseline_year_draws <- cell_pred[which(cell_pred[, 1] == year_idx), ] ## first col is year
          baseline_year_draws <- baseline_year_draws[, -(1:2)]

          # Now do the comparisons
          if (target_type == "greater") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws >= 1 - target, 1, 0)
          } else if (target_type == "less") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws <= 1 - target, 1, 0)
          }
          relative_proj_draws[is.na(relative_proj_draws)] <- 0
          relative_goal_prob <- rowMeans(relative_proj_draws)

          # Save
          message("-- Saving probabilities...")
          saveRDS(
            object = relative_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_vs_",
              baseline_year, "_", abs_rel, "_", target_type, "_", target,
              "_cell_target_probs_", regions[ii], ".RDs"
            )
          )

          rm(relative_goal_prob, relative_proj_draws)
        } # close abs/relative if/then/else
      } # close regions loop
    } # close cell loop

    if (grepl("admin", pred_type, fixed = TRUE)) {
      message("Working on ADMIN level")

      # Load sp hierarchy
      sp_h <- get_sp_hierarchy(shapefile_version = shapefile_version)

      for (aa in 0:2) { ## for each admin type

        # Load proj_draws object
        message("-- Loading proj_draws object...")
        proj_draws <- readRDS(sprintf(
          "%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs",
          proj_dir, ind, measure, target_year, aa,
          ifelse(uselogit, "_logit", "")
        ))

        # Split off spatial index
        spatial_idx <- proj_draws[, 1]
        proj_draws <- proj_draws[, 2:ncol(proj_draws)]

        # Generate probabilities
        if (abs_rel == "absolute") {
          if (target_type == "greater") {
            absolute_goal_draws <- ifelse(proj_draws >= target, 1, 0)
          } else if (target_type == "less") {
            absolute_goal_draws <- ifelse(proj_draws <= target, 1, 0)
          }

          absolute_goal_draws[is.na(absolute_goal_draws)] <- 0
          absolute_goal_prob <- rowMeans(absolute_goal_draws)

          # Add spatial index
          absolute_goal_prob <- cbind(spatial_idx, absolute_goal_prob)

          # Save
          message("-- Saving probabilities...")

          # RDS
          saveRDS(
            object = absolute_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
              target_type, "_", target, "_adm_", aa, "_target_probs.RDs"
            )
          )

          # CSV
          absolute_goal_prob <- merge_sp_hierarchy(
            df = absolute_goal_prob,
            admin_level = aa,
            idx_col = "spatial_idx",
            sp_h = sp_h
          )

          write.csv(absolute_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_", abs_rel, "_",
              target_type, "_", target, "_adm_", aa, "_target_probs.csv"
            ),
            row.names = F
          )
        }

        ## Did it meet relative goal?
        if (abs_rel == "relative") {
          ## load the admin objects
          ## try two different locations until we standardize
          message("- loading admin objects")
          file_1 <- sprintf(
            "/share/geospatial/mbg/%s/%s/output/%s/%s_%s_admin_draws_raked.Rdata",
            ind_gp, ind, rd, ind, measure
          )
          file_2 <- paste0(
            "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/",
            ind, "_raked_admin_draws_eb_bin0_0.RData"
          )

          if (file.exists(file_1)) {
            load(file_1)
          } else if (file.exists(file_2)) {
            load(file_2)
          } else {
            stop("Cannot load admin pred object!")
          }

          # Need to load "pseudo cell_pred" admin object to get baseline year
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

          # Assess relative goals
          if (target_type == "greater") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws >= 1 - relative_goal, 1, 0)
          } else if (target_type == "less") {
            relative_proj_draws <- ifelse(proj_draws / baseline_year_draws <= 1 - relative_goal, 1, 0)
          }
          relative_proj_draws[is.na(relative_proj_draws)] <- 0
          relative_goal_prob <- rowMeans(relative_proj_draws)

          # Add spatial index
          relative_goal_prob <- cbind(spatial_idx, relative_goal_prob)

          message("-- Saving probabilities...")
          saveRDS(
            object = relative_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_vs_",
              baseline_year, "_", abs_rel, "_", target_type, "_", target,
              "_adm_", aa, "_target_probs.RDs"
            )
          )

          # CSV
          relative_goal_prob <- merge_sp_hierarchy(
            df = relative_goal_prob,
            admin_level = aa,
            idx_col = "spatial_idx",
            sp_h = sp_h
          )

          write.csv(relative_goal_prob,
            file = paste0(
              output_dir, ind, "_", measure, "_", target_year, "_vs_",
              baseline_year, "_", abs_rel, "_", target_type, "_", target,
              "_adm_", aa, "_target_probs.csv"
            ),
            row.names = F
          )
        } # close relative loop
      } # close admin loop
    } # close if_admin section
  } # close goal_obj row loop
}
