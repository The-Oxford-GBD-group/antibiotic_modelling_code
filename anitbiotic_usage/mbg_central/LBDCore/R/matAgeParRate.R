#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ceb PARAM_DESCRIPTION
#' @param age_group PARAM_DESCRIPTION
#' @param groups PARAM_DESCRIPTION, Default: c("15-19", "20-24", "25-29", "30-34")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname matAgeParRate
#' @export
matAgeParRate <- function(ceb,
                          age_group,
                          groups = c("15-19", "20-24", "25-29", "30-34")) {

  # functions to determine proportion of women in the given age group

  # given vectors `ceb` and `age_group` reporting the number of children ever
  # born to mothers and the age groups to which they belong,
  # return the proportion of births falling in each of the age groups in
  # `groups`.

  # check it's a character vector
  stopifnot(class(ceb) %in% c("numeric", "integer"))
  stopifnot(class(age_group) == "character")

  # count the number of births in all age groups
  counts <- tapply(ceb, age_group, sum)

  # add on any groups not represented a 0s
  missing_groups <- which(!(groups %in% names(counts)))

  if (length(missing_groups) > 0) {
    dummy_counts <- rep(0, length(missing_groups))
    names(dummy_counts) <- groups[missing_groups]
    counts <- c(counts, dummy_counts)
  }

  # get proportions
  props <- counts / sum(counts)

  # find the ones we want
  idx_keep <- match(groups, names(props))

  # and return these
  return(props[idx_keep])
}
