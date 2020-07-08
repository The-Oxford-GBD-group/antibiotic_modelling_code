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
#' @rdname age_sex_spec
#' @export
age_sex_spec <- function(df) {
  demos <- detect_demographics(df)
  by_age <- ifelse(22 %in% demos$age_group_id, 0, 1)
  by_sex <- ifelse(3 %in% demos$sex_id, 0, 1)
  spec <- cbind(by_age, by_sex) %>% data.table()
  return(spec)
}
