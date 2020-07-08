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
#' @rdname detect_demographics
#' @export
detect_demographics <- function(df) {
  vars <- c("location_id", "year_id", "age_group_id", "sex_id")
  ## Check if data frame has id's and only return on those that exist
  vars <- names(df)[names(df) %in% vars]
  demos <- lapply(vars, function(x) unique(df[[x]]))
  names(demos) <- vars
  return(demos)
}
