#' @title Run-time to hours
#'
#' @description Convert a validated \code{DD:HH:MM:SS} runtime string to hours
#'
#' @param run_time String. Run-time in format \code{DD:HH:MM:SS}.
#'
#' @return number of hours
#'
#' @export
dhms_to_hours <- function(run_time) {

  ## Split into numeric vector
  rt_split <- as.numeric(stringr::str_split(
    run_time, ":"
  )[[1]])

  ## Convert to hours space
  return(rt_split[1] * 24 + rt_split[2] * 1 + rt_split[3] / 60 + rt_split[4] / 3600)
}
