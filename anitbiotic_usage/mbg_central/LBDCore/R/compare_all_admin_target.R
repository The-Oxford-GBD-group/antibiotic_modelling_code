#' @title Runs comparison of projected years against a goal that is meant to be
#' applied to all admins within a given country.
#'
#' @description This is somewhat of a special use scenario but does have some specific applications.
#' For instance, the GVAP goal for geographic parity in vaccine coverage is "All
#' districts with coverage > 80\% within a country by 2020.  The proper way to
#' calculate the probability of achieving this goal is to calculate by draw
#' whether the country achieved the goal in the given draw, then
#' see in how many draws the country achieved the goal.  That is what this
#' function does.
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param measure prevalence, incidence, mortality, etc
#' @param target_year Year for which the goal is meant. Can be either a future (projected) year
#'   with the year projections  already  generated for this target_year using \code{make_proj()},
#'   or can be a modeled year (depending on the \code{proj} parameter below.
#' @param target Target (e.g. 0.8 for 80\% vaccine coverage); numeric
#' @param target_type Type of target ('greater' or 'less'); character
#' @param admin_level What admin level should the target be evaluated at? For instance, use
#'   \code{1} if the goal applies to all admin1s within a given country, or \code{2} if
#'   all the goal applies to all admin2s within a given country
#' @param uselogit Should this be done in logit space?
#' @param proj Is \code{target_year} projected (\code{proj = T}) or modeled (\code{proj = F})?
#' @param shapefile_version character string indicating version of shapefile to pull
#' @return A data table by country with probability of meeting the specified goal
#' @examples
#' \dontrun{
#' # Compare against all-admin2 > 80% target for GVAP
#' compare_all_admin_target(
#'   ind_gp = indicator_group,
#'   ind = indicator,
#'   rd = run_date,
#'   measure = "prevalence",
#'   target_year = 2020,
#'   target = 0.8,
#'   target_type = "greater",
#'   admin_level = 2,
#'   uselogit = T,
#'   proj = T
#' )
#' }
#' @export
compare_all_admin_target <- function(ind_gp,
                                     ind,
                                     rd,
                                     measure,
                                     target_year,
                                     target,
                                     target_type,
                                     admin_level,
                                     uselogit,
                                     proj,
                                     shapefile_version = "current") {

  # Load sp hierarchy
  sp_h <- get_sp_hierarchy(shapefile_version = shapefile_version)

  # Define directories
  share_dir <- paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/")
  proj_dir <- paste0(share_dir, "/pred_derivatives/proj/")

  # Convenience
  aa <- admin_level

  if (proj == T) {

    # Load proj_draws object if working with projected data
    message("-- Loading proj_draws object...")
    proj_draws <- readRDS(sprintf(
      "%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs",
      proj_dir, ind, measure, target_year, aa,
      ifelse(uselogit, "_logit", "")
    ))
  } else if (proj == F) {

    # Load admin preds object if working with non-projected data
    # That is, if year is within the modeling frame

    message("Working on ADMIN level")
    ## load the admin objects
    ## try two different locations until we standardize
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

    # Create a proj_draws object (same name for convenience)
    proj_draws <- get(paste0("admin_", aa))
    proj_draws <- subset(proj_draws, year == target_year)

    # Format to be the same as the other proj_draws object
    proj_draws <- subset(proj_draws, select = names(proj_draws)[!(names(proj_draws) %in% c("year", "pop"))])
    setnames(proj_draws, paste0("ADM", aa, "_CODE"), "spatial_idx")
  }

  proj_draws <- as.data.table(proj_draws)

  # Drop NAs or NaNs and warn
  missing_values <- proj_draws[which(complete.cases(proj_draws) == F), "spatial_idx"]
  if (length(missing_values) > 0) {
    message("Warning: the following ADM_CODEs are NA or NaN and will be dropped:")
    print(missing_values)
    proj_draws <- proj_draws[which(complete.cases(proj_draws) == T)]
  }

  # Split off spatial index
  spatial_idx <- proj_draws[, 1]
  proj_draws <- proj_draws[, 2:ncol(proj_draws)]

  # Generate admin_level goals
  if (target_type == "greater") {
    absolute_goal_draws <- ifelse(proj_draws >= target, 1, 0)
  } else if (target_type == "less") {
    absolute_goal_draws <- ifelse(proj_draws <= target, 1, 0)
  }

  absolute_goal_draws[is.na(absolute_goal_draws)] <- 0

  # Switch to data table
  absolute_goal_draws <- as.data.table(absolute_goal_draws)

  # Add spatial index back on
  absolute_goal_draws <- cbind(spatial_idx, absolute_goal_draws)

  # Merge the rest of the info on
  absolute_goal_prob <- merge_sp_hierarchy(
    df = absolute_goal_draws,
    admin_level = aa,
    idx_col = "spatial_idx",
    sp_h = sp_h
  )

  # First split into a list by country, keeping only admin0 levels
  absolute_goal_list <- absolute_goal_prob %>%
    subset(., select = c("ADM0_CODE", "ADM0_NAME", names(.)[grep("V.*", names(.))])) %>%
    split(., by = "ADM0_CODE")

  # Check to see if, within draw, meeting goal for all admins in country
  adm_table_list <- lapply(absolute_goal_list, function(adm_table) {
    n <- nrow(adm_table)

    # Grab admin info
    adm_name <- adm_table[, 1:2] %>% unique()

    # Check if meets criteria in each draw
    adm_table <- adm_table[, 3:ncol(adm_table)] %>%
      summarize_all(sum)

    adm_table <- ifelse(adm_table == n, 1, 0)
    adm_table <- rowMeans(adm_table)
    adm_table <- cbind(adm_name, as.data.table(adm_table))
    return(adm_table)
  })

  adm_table_summary <- do.call(rbind, adm_table_list) %>%
    as.data.table() %>%
    setnames(., "adm_table", "probability")

  return(adm_table_summary)
}
