#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getCols
#' @export
getCols <- function(df, period = 1) {
  # subset results matrix
  df[, grep(
    sprintf("^p%s_", period),
    colnames(df)
  )]
}
