#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param age_yr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname gbd_age
#' @export
gbd_age <- function(df, age_yr) {

  ## Age groups
  df[, age_group_id := round((age_yr + 25) / 5)]
  df[age_yr > 80, age_group_id := 21]

  return(df)
}
