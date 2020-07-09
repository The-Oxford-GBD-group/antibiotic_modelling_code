#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param v PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cfb
#' @export
cfb <- function(v) {

  # Calculate the Coffey-Feingold-Bromberg metric for
  # a numeric vector `v` - useful as a normed measure
  # of variability for a set of proportions.

  # This implementation assumes equal weights

  # Reference:
  # Coffey, M. P., Feingold, M., & Bromberg, J. (1988).
  # A normed measures of variability among proportions.
  # Computational Statistics & Data Analysis, 7(2), 127-141.
  # https://doi.org/10.1016/0167-9473(88)90088-6

  # calculate mean (u) and sample size (n)
  u <- mean(v, na.rm = T)
  if (is.na(u)) return(NA)
  n <- length(v)

  # define numerator (h)
  h <- var(v)

  # calculate denominator (max h)
  # (assuming equal weights)
  r <- n * u - floor(n * u)
  h_max <- u * (1 - u) - r * (1 - r) / n

  # return H statistic (sqrt(h/h_max))
  return(sqrt(h / h_max))
}
