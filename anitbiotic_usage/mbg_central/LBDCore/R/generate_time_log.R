#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ticlog PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname generate_time_log
#' @export
generate_time_log <- function(ticlog) {
  ## General usage:
  ##    require(tictoc)
  ##
  ##    tic("Step 1")
  ##    **your code here**
  ##    toc(log = T)
  ##
  ##    ticlog <- tic.log(format = F)
  ##    generate_time_log(ticlog)
  ##
  ##  Returns: data table with two columns
  ##     "step": names of events (e.g. "Step 1")
  ##     "time": time elapsed (as text: Xh Xm Xs)
  ##
  ##  Note: can nest tic/toc pairs
  # Functions in functions
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


  df_out <- lapply(ticlog, strip_time) %>% rbindlist()

  return(df_out)
}
