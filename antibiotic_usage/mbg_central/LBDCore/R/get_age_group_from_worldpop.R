#' @title Get GBD age group for the given WorldPop measure
#' @description This function will return the age group from the GBD age group ID table
#' which corresponds to the WorldPop measures used
#'
#' @note Currently only taking in measures starting with "a" and for 12 month interval (ending with "t"). And \code{total}.
#'
#' @param pop_measure The WorldPop measure, e.g. \code{a0004t}. Default: \code{a0004t}
#'
#' @return The \code{age_group_id} for the associated \code{pop_measure} input.
#'
#' @rdname get_age_group_from_worldpop
#' @export
get_age_group_from_worldpop <- function(pop_measure) {

  ## Strsplit out the 4 digits from the middle (always fixed length),
  ## only if its not total
  age_measures <- pop_measure
  if (pop_measure != "total") {
    age_measures <- substr(x = pop_measure, start = 2, stop = 5)
  }

  ## NOT ALL the ages have perfect 1:1 measure, and so we may
  ## have to aggregate out some age groups

  ## Here's a dictionary of age mappings
  age_dict <- list(
    "0004" = c(2, 3, 4, 5),
    "0514" = c(23),
    "1014" = c(7),
    "1519" = c(8),
    "1549" = c(24),
    "2024" = c(9),
    "2529" = c(10),
    "3034" = c(11),
    "3539" = c(12),
    "4044" = c(13),
    "4549" = c(14),
    "5054" = c(15),
    "5559" = c(16),
    "6064" = c(17),
    "total" = c(22)
  )

  ## Lookup the GBD age group for the given WorldPop measure and return it
  return(unlist(age_dict[paste0(age_measures)]))
}
