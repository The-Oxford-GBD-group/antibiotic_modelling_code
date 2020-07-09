#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#'
#' @note THIS FUNCTION HAS BEEN DEPRECATED IN FAVOR OF SET_UP_CONFIG
#'
#' @param repo Location where you've cloned "mbg" repository.
#' @param indicator_group Category of indicator, i.e. "education"
#' @param indicator Specific outcome to be modeled within indicator category, i.e. "edu_0"
#' @param config_name PARAM_DESCRIPTION, Default: NULL
#' @param covs_name PARAM_DESCRIPTION, Default: NULL
#' @param post_est_only PARAM_DESCRIPTION, Default: FALSE
#' @param run_date PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname set_up_config
#' @export
load_config <- function(...) {
  warning("load_config() and check_config() will be deprecated in favor of set_up_config()")
  set_up_config(...)
}
