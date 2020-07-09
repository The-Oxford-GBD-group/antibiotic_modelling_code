#' @title Aggregate raked linked cell pred
#'
#' @description rake linked cell pred to admins0-2, maintaining draws
#'
#' @param raked_linked_cell_pred a raked and linked cell pred object
#' @param year_list list of years in cell_pred
#' @param raked Boolean, raked or unraked cell draws
#'
#' @return a list with 3 data.tables for admins0-2 with draws intact
#'
#' @export
aggregate_counts <- function(linked_cell_pred, overs, raked) {
  admin_2 <- linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year', 'ADM2_CODE', 'ADM0_CODE')]
  admin_1 = linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year','ADM1_CODE', 'ADM0_CODE')]
  admin_0 = linked_cell_pred[, lapply(overs, function(x) sum(get(x), na.rm = T)), by = c('year','ADM0_CODE')]
  if (raked) {
    return(list("raked_adm0_draws" = admin_0, "raked_adm1_draws" = admin_1,"raked_adm2_draws" = admin_2))
  } else {
    return(list("unraked_adm0_draws" = admin_0, "unraked_adm1_draws" = admin_1,"unraked_adm2_draws" = admin_2))
  }
}
