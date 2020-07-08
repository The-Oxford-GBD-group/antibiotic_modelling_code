#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param country PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname approx_match
#' @export
approx_match <- function(country) {
  # First, try matching to long form of name
  gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name), ]$GAUL_CODE

  # If that doesn't work, grep within the short name
  if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_nm_sh), ]$GAUL_CODE

  # If that doesn't work, grep within the long name
  if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name), ]$GAUL_CODE

  # Could fill in other matching here if desired

  # Warn if nonspecific
  if (length(gaul_code) > 1) warning(paste0("\"", country, "\" matches multiple country names in the lookup table. Please be more specific."))

  # Finally, if no matches, return NA
  if (length(gaul_code) != 1) gaul_code <- NA

  return(as.numeric(gaul_code))
}
