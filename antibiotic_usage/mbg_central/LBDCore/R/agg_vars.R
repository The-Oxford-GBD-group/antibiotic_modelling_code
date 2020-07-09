#####################################################################################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param location_set_version_id PARAM_DESCRIPTION
#' @param vars PARAM_DESCRIPTION
#' @param parent_ids PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname agg_vars
#' @export
agg_vars <- function(df, location_set_version_id, vars, parent_ids) {

  ## Merge on populations if need be
  if (!("pop_scaled" %in% names(df))) {
    demos <- detect_demographics(df)
    year_start <- min(demos$year_id)
    year_end <- max(demos$year_id)
    custom_sex_id <- demos$sex_id
    custom_age_group_id <- demos$age_group_id
    pops <- get_populations(location_set_version_id, year_start, year_end, custom_sex_id = custom_sex_id, custom_age_group_id = custom_age_group_id)
    df <- merge(df, pops, by = c("location_id", "year_id", "age_group_id", "sex_id"))
  }

  ## Merge on parent_ids if need be
  if (!("parent_id" %in% names(df))) {
    locs <- get_location_hierarchy(location_set_version_id)[, .(location_id, parent_id)]
    df <- merge(df, locs, by = "location_id")
  }

  ## Subset to requested parent_ids
  df <- df[parent_id %in% parent_ids]

  ## Aggregate estimates to the parent_id [ sum(var * pop) /sum(pop) ]
  df[, (vars) := lapply(.SD, function(x) sum(x * df[["pop_scaled"]]) / sum(df[["pop_scaled"]])), .SDcols = vars, by = c("parent_id", "year_id", "age_group_id", "sex_id")]
  ## De-duplicate so get one set of estimates
  df <- unique(df[, c("parent_id", "year_id", "age_group_id", "sex_id", vars), with = F])
  ## Rename parent_id -> location_id
  setnames(df, "parent_id", "location_id")

  return(df)
}
