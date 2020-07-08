#' @title Compare admin years
#' @description Compare two admin years: what was the difference between them (with CIs)?
#'
#' @param ind_gp indicator group
#' @param ind indicator
#' @param rd run_date
#' @param measure prevalence, incidence, mortality, etc
#' @param start_year baseline year for analysis
#' @param end_year comparison year for analysis
#' @param admin_level What admin level should the target be evaluated at?
#' @param uselogit Were projections created in logit space?
#' @param proj Is \code{target_year} projected (\code{proj = T}) or modeled (\code{proj = F})?
#' @param shapefile_version character string indicating version of shapefile to pull
#' @return A data table by admin unit with summary stats summarized for the difference
#' @examples
#' \dontrun{
#' difference_df <- compare_admin_years(
#'   ind_gp = indicator_group,
#'   ind = indicator,
#'   rd = run_date,
#'   measure = "prevalence",
#'   start_year = 2000,
#'   end_year = 2015,
#'   admin_level = 2,
#'   uselogit = T,
#'   year_list = year_list,
#'   summstats = c("mean", "upper", "lower", "cirange")
#' )
#' }
#' @export
compare_admin_years <- function(ind_gp,
                                ind,
                                rd,
                                measure,
                                start_year,
                                end_year,
                                admin_level,
                                uselogit,
                                year_list,
                                summstats = c("mean", "upper", "lower", "cirange"),
                                shapefile_version = "current") {

  # Load sp hierarchy
  sp_h <- get_sp_hierarchy(shapefile_version = shapefile_version)

  # Define directories
  share_dir <- paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/")
  proj_dir <- paste0(share_dir, "/pred_derivatives/proj/")

  # Convenience
  aa <- admin_level

  yr1 <- get_admin_preds(start_year, year_list)
  yr2 <- get_admin_preds(end_year, year_list)

  if (!all.equal(yr1$spatial_idx, yr2$spatial_idx)) stop("Spatial indices of year admin draw objects do not match!")

  # Split off spatial index
  spatial_idx <- yr1[, 1]
  yr1 <- as.matrix(yr1[, 2:ncol(yr1)])
  yr2 <- as.matrix(yr2[, 2:ncol(yr2)])

  if (!all.equal(dim(yr1), dim(yr2))) stop("Dimensions of year admin draw objects do not match!")

  # Calculate the draw-wise difference
  diff <- yr2 - yr1

  # Create an admin draws object
  ad_draws <- cbind(
    year = rep(paste0(end_year, " - ", start_year), length(spatial_idx)),
    spatial_idx,
    diff,
    pop = rep(NA, length(spatial_idx))
  )

  ad_draws <- as.data.table(ad_draws)

  setnames(ad_draws, "spatial_idx", paste0("ADM", aa, "_CODE"))

  # Summarize over summstats
  out_df <- make_admin_pred_summary(
    admin_pred = ad_draws,
    sp_hierarchy_list = sp_h,
    summary_stats = summstats
  )

  return(out_df)
}
