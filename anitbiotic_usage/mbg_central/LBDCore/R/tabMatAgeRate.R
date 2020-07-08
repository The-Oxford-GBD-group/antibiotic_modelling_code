
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param age_group PARAM_DESCRIPTION
#' @param cluster_id PARAM_DESCRIPTION
#' @param groups PARAM_DESCRIPTION, Default: c("15-19", "20-24", "25-29", "30-34")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname tabMatAgeRate
#' @export
tabMatAgeRate <- function(age_group,
                          cluster_id,
                          groups = c("15-19", "20-24", "25-29", "30-34")) {

  # function to tabulate maternal age rate
  # given a vector `age_group` reporting the age group to which mothers belong,
  # and a vector `cluster_id` giving the cluster to which each mother belongs
  # return a matrix - with number of rows equal to the number of unique elements
  # in `cluster_id` and number of columns equal to the length of `groups` -
  # giving the proportion falling in each of the age groups in `groups`.

  # get the grouped data as a list
  ans <- tapply(
    age_group,
    cluster_id,
    matAgeRate,
    groups
  )

  # combine into a matrix
  ans <- do.call(rbind, ans)

  # and return
  return(ans)
}
