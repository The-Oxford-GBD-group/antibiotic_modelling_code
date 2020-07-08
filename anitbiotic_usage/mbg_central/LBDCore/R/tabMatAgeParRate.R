#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ceb PARAM_DESCRIPTION
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
#' @rdname tabMatAgeParRate
#' @export
tabMatAgeParRate <- function(ceb,
                             age_group,
                             cluster_id,
                             groups = c("15-19", "20-24", "25-29", "30-34")) {

  # function to tabulate maternal age rate

  # given vectors `ceb` and `age_group` reporting the number of children ever
  # born to mothers and the age groups to which they belong,
  # return a matrix - with number of rows equal to the number of unique elements
  # in `cluster_id` and number of columns equal to the length of `groups` -
  # giving the proportion of births falling in each of the age groups in
  # `groups`.

  # get unique cluster ids
  cluster_ids <- unique(cluster_id)

  # create dummy results matrix
  ans <- matrix(NA,
    nrow = length(cluster_ids),
    ncol = length(groups)
  )

  rownames(ans) <- cluster_ids
  colnames(ans) <- groups

  # loop through the cluster ids calculating the rates
  for (i in 1:length(cluster_ids)) {

    # get index for the cluster
    idx_cluster <- which(cluster_id == cluster_ids[i])

    ans[i, ] <- matAgeParRate(
      ceb = ceb[idx_cluster],
      age_group = age_group[idx_cluster],
      groups = groups
    )
  }

  # and return
  return(ans)
}
