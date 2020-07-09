#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#' @rdname setup_design
#' @export
setup_design <- function(df, var) {

  ## Set options

  ## conservative adjustment recommended by Thomas Lumley for single-PSU strata.  Centers the data for the single-PSU stratum around the sample grand mean rather than the stratum mean
  options(survey.lonely.psu = "adjust")

  ## conservative adjustment recommended by Thomas Lumley for single-PSU within subpopulations.  Need to find out more about what exactly this is doing.
  options(survey.adjust.domain.lonely = TRUE)

  ## Check for survey design vars
  check_list <- c("strata", "psu", "pweight")
  for (i in check_list) {
    ## Assign to *_formula the variable if it exists and nonmissing, else NULL
    assign(
      paste0(i, "_formula"),
      ifelse(i %in% names(df) & nrow(df[!is.na(i)]) > 0, paste("~", i), NULL) %>% as.formula()
    )
  }

  ## Set svydesign
  return(svydesign(id = psu_formula, weight = pweight_formula, strat = strata_formula, data = df[!is.na(var)], nest = TRUE))
}
