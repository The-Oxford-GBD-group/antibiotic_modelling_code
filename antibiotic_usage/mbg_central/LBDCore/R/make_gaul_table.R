#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_gaul_table
#' @export
make_gaul_table <- function(rr) {
  data.table(
    region = rr,
    gaul_code = get_adm0_codes(rr, shapefile_version = shapefile_version)
  )
}
