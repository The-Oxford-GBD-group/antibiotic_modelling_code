#' @title FUNCTION_TITLE
#' @description function to update the fixed effects variable
#' @param effects_equation PARAM_DESCRIPTION, Default: fixed_effects
#' @param cat_var PARAM_DESCRIPTION, Default: ''
#' @param cat_ras PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname adjust_equation_categorical
#' @export
adjust_equation_categorical <- function(effects_equation = fixed_effects, cat_var = "", cat_ras) {
  # format the raster names into an equation format
  eq_add <- paste0(names(cat_ras), collapse = " + ")
  effects_equation <- sub(cat_var, eq_add, effects_equation)

  return(effects_equation)
}
