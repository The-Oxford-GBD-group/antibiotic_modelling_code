#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname strip_time
#' @export
strip_time <- function(x) {
  sec <- as.numeric(x$toc - x$tic)
  time <- format_time(sec)
  name <- x$msg

  df <- c(name, time) %>%
    t() %>%
    as.data.table()

  names(df) <- c("step", "time")

  return(df)
}
