#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname check_for_holdouts
#' @export
check_for_holdouts <- function(dir) {
  cell_draw_files <- list.files(dir, pattern = "_cell_draws_")
  n_holdouts <- gsub(".*_([0-9]+).RData", "\\1", cell_draw_files) %>%
    as.numeric() %>%
    max()
}
