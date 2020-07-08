#################################################################################
### Split country/years (in format 'country_year') out of rownames of a condSim df
## Inputs:
# condSim_object: data matrix that make_condSim() spits out
## Outputs: returns matrix with name and year in two columns of same length
#################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param condSim_object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname split_geo_names
#' @export
split_geo_names <- function(condSim_object) {
  splits <- strsplit(gsub("C\xf4te d'Ivoire", "Cote dIvoire", rownames(condSim_object)), split = "_")
  ctry <- sapply(splits, "[", 1)
  year <- as.numeric(sapply(splits, "[", 2))
  ret <- cbind(
    "name" = ctry,
    "year" = as.numeric(year)
  )
  rownames(ret) <- NULL
  return(ret)
}
