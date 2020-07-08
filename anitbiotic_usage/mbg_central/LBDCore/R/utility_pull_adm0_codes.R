#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param region_codes PARAM_DESCRIPTION
#' @param standard_regex PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname utility_pull_adm0_codes
#' @export
utility_pull_adm0_codes <- function(region_codes, standard_regex) {

  ## PROCESS NON-CUSTOM REGIONS (MBG MODELING REGIONS + ISOS) ~~~~~~~~~~~~~~~~~~
  # Helper function to pull all GAUL codes for a set of modeling regions or
  #  ISO codes

  # Return none if the length of the inputs is 0
  if (length(region_codes) == 0) return(integer(0))
  # Input data assertions
  if (!all(grepl(standard_regex, region_codes))) {
    stop("All region codes must match the MBG or ISO code formats.")
  }
  isos <- region_codes[nchar(region_codes) != 4]
  mbg_regs <- region_codes[nchar(region_codes) == 4]
  if ("all" %in% isos) {
    # 'all' is a special case where all GAUL codes are pulled
    pulled_adm0s <- unique(lookup_table[get(pull_field) >= 0, get(pull_field)])
  } else {
    (
      # Pull all GAUL codes matching the ISOs or regions
      pulled_adm0s <- unique(
        lookup_table[
          ((iso3 %in% isos) | (mbg_reg %in% mbg_regs)) & (get(pull_field) >= 0),
          get(pull_field)
        ]
      )
    )
  }
  # Warn if any iso codes or MBG regions aren't in the lookup table
  missing_isos <- isos[ !(isos %in% lookup_table[, iso3]) & (isos != "all") ]
  if (length(missing_isos) > 0) {
    message(paste0(
      "WARNING: Missing these ISOs: ",
      paste(missing_isos, collapse = ",")
    ))
  }
  missing_regs <- mbg_regs[ !(mbg_regs %in% lookup_table[, mbg_reg]) ]
  if (length(missing_regs) > 0) {
    message(paste0(
      "WARNING: Missing these MBG regions: ",
      paste(missing_regs, collapse = ",")
    ))
  }
  return(as.integer(pulled_adm0s))
}
