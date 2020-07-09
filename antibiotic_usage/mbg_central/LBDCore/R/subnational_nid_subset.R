#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname subnational_nid_subset
#' @export
subnational_nid_subset <- function(df) {
  df %>%
    mutate(point = as.numeric(levels(point))[point]) %>%
    rowwise() %>%
    mutate(point = ifelse(svy_id %in% subnational_nids, point + 2, point)) %>%
    ungroup() %>%
    mutate(point = as.factor(point)) %>%
    data.table()
}
