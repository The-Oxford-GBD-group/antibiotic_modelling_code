#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname pull_period_pops
#' @export
pull_period_pops <- function(period) {
  geo.subset <- geo.dt[period_index == paste0("period_", period), ]
  geo.subset <- geo.subset[, pops := raster::extract(country_pops[[period]], cell_idx)]
  geo.subset <- geo.subset[, admin2 := raster::extract(country_admin2, cell_idx)]
  return(geo.subset)
}
