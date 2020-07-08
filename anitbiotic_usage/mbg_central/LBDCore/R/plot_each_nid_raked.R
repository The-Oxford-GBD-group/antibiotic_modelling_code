#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param this_nid PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot_each_nid_raked
#' @export
plot_each_nid_raked <- function(this_nid) {
  raked_dhs_plot <- compare_admin_estimates(
    nid_list = this_nid,
    results_raster = results_raked,
    compare_source_title = "DHS Admin1",
    raked_title = "raked",
    outcome_title = "Mean years",
    compare_data = dhs_admin1_data,
    compare_spdf = poly_shapes_all,
    master_list = master_list
  )
  return(raked_dhs_plot)
}
