#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
#' @rdname matAgeRate
#' @export
matAgeRate <- function(age_group,
                       groups = c("15-19", "20-24", "25-29", "30-34")) {

  # functions to determine proportion of women in the given age group

  # given a vector `age_group` reporting the age group to which mothers belong,
  # return the proportion falling in each of the age groups in `groups`.

  # check it's a character vector
  stopifnot(class(age_group) == "character")

  # count the number in all age groups
  counts <- table(age_group)

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
