#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_names
#' @export
get_names <- function(df) {

  ## Given a data.table with location, age_group, sex, merge on namescu
  required <- c("location_id", "age_group_id", "sex_id")
  missing <- required[!required %in% names(df)]
  if (length(missing) > 0) stop(paste0("Missing required columns: ", toString(required)))

  ## Detect what needs names
  cols <- c("ihme_loc_id", "location_name", "age_group_name", "sex")
  need <- cols[!cols %in% names(df)]

  ## Names
  if ("ihme_loc_id" %in% need) df <- merge(df, get_location_hierarchy(41)[, .(location_id, ihme_loc_id)], by = "location_id", all.x = T)
  if ("location_name" %in% need) df <- merge(df, get_location_hierarchy(41)[, .(location_id, location_name)], by = "location_id", all.x = T)
  if ("age_group_name" %in% need) df <- merge(df, get_ages()[, .(age_group_id, age_group_name)], by = "age_group_id", all.x = T)
  if ("sex" %in% need) df <- merge(df, get_sexes()[, .(sex_id, sex)], by = "sex_id", all.x = T)

  return(df)
}
