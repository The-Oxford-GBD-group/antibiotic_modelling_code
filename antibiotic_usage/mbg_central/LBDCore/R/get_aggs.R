
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param agg_geo_est PARAM_DESCRIPTION, Default: cond_sim_adm0
#' @param gaul_list PARAM_DESCRIPTION, Default: gaul_list
#' @param rake_to PARAM_DESCRIPTION, Default: gbd
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_aggs
#' @export
get_aggs <- function(agg_geo_est = cond_sim_adm0,
                     gaul_list = gaul_list,
                     rake_to = gbd) {

  #################################################################################
  ### Generates aggregated estimates in case of logit raking
  ## Modified from calc_raking_factors()
  #################################################################################

  message("WARNING: function will not work as expected if agg_geo_est and rake_to are not aggregated at the same admin level.")

  if (sum(colnames(rake_to) %in% c("name", "year", "mean")) != 3) {
    stop("rake_to should be a data table with column names `name`, `year`, and `mean`")
  }

  if (dim(t(as.matrix(agg_geo_est)))[1] != 1 | is.null(names(agg_geo_est))) {
    stop("agg_geo_est must be a named vector returned from make_condSim() with summarize option equal to TRUE")
  }

  # transpose and rename agg_geo_est
  agg_geo_est <- data.table(split_geo_names(as.matrix(agg_geo_est)), agg_geo_est)
  agg_geo_est$year <- as.numeric(agg_geo_est$year)
  agg_geo_est$name <- as.numeric(agg_geo_est$name)

  # merge
  merged <- merge(agg_geo_est, rake_to, by = c("name", "year"), all.x = T)
  names(merged)[names(merged) == "agg_geo_est"] <- "geo_mean"
  names(merged)[names(merged) == "mean"] <- "rake_to_mean"

  return(merged)
}
