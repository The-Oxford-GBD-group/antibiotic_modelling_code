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
#' @rdname china_hierarchy_fix
#' @export
china_hierarchy_fix <- function(df) {

  ## Reroute China subnationals to level 4, make CHN w/o HKG and MAC, HKG, MAC level 3
  ## Remove China
  df <- df[location_id != 6, ]
  ## Remove from path to top parent
  df <- df[, path_to_top_parent := gsub(",6,", ",", path_to_top_parent)]
  ## Reroute 44533, 354, 361 to level 3
  df <- df[location_id %in% c(44533, 354, 361), level := 3]
  ## Reroute the other subnationals to level 4
  df <- df[grepl("CHN", df$ihme_loc_id) & level == 5, level := 4]

  return(df)
}
