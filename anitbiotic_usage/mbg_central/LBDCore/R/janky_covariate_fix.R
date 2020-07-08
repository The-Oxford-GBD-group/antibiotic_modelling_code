#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' ## 4749, 44533 fix to create if missing
#' @param df PARAM_DESCRIPTION
#' @param var PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname janky_covariate_fix
#' @export
janky_covariate_fix <- function(df, var) {

  ## Merge on populations
  spec <- age_sex_spec(df)
  year_start <- min(df$year_id)
  year_end <- max(df$year_id)
  pops <- get_populations(41, year_start, year_end, spec$by_sex, spec$by_age)
  df <- merge(df, pops, by = c("location_id", "year_id", "sex_id", "age_group_id"))
  ## Aggregate
  if (!(4749 %in% unique(df$location_id))) {
    gbr_4749 <- agg_vars(df, location_set_version_id = 41, vars = var, parent_id = 4749)
    df <- rbind(df, gbr_4749, fill = T)
  }
  if (!(44533 %in% unique(df$location_id))) {
    chn_44533 <- agg_vars(df, location_set_version_id = 41, vars = var, parent_id = 44533)
    df <- rbind(df, chn_44533, fill = T)
  }
  # Drop the pop_scaled column
  df[, pop_scaled := NULL]

  return(df)
}
