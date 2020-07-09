#' @title Complete user-supplied runtime string into DHMS format
#'
#' @description Check the input string of runtime and convert its values into a DD:HH:MM:SS format
#'
#' @param run_time String. User supplied run-time in format \code{[DD]:[HH]:[MM]:[SS]}
#'
#' @return The same run-time supplied by user once validations have passed,
#' but with days appended if not supplied, in format \code{DD:HH:MM:SS}
#'
#' @importFrom stringr str_split
#' @export
complete_runtime_string <- function(run_time) {

  ## Split runtime into its components
  ## We could have 3 (H:M:S) or 4 (D:H:M:S) components here
  rt_split <- as.numeric(stringr::str_split(run_time, ":")[[1]])


  ## Validate for the entries in the timestamp
  ## and return a 'complete' timestamp
  ## with 'DD' added if the validations are correct

  ## DD:HH:MM:SS
  if (length(rt_split) == 4) {

    ## Check that days is between 0-25
    stopifnot(rt_split[1] %in% c(0:25))

    ## No restriction on hours

    ## Check that both minutes and seconds
    ## are between 0-59
    stopifnot(rt_split[3] %in% c(0:59), rt_split[4] %in% c(0:59))

    return(run_time)
  } else if (length(rt_split) == 3) {

    ## HH:MM:SS
    ## Check that both minutes and seconds
    ## are between 0-59
    stopifnot(rt_split[1] %in% c(0:59), rt_split[2] %in% c(0:59))

    return(paste0("00:", run_time))
  } else if (length(rt_split) == 2) {

    ## HH:MM:SS
    ## Check that both minutes and seconds
    ## are between 0-59
    stopifnot(rt_split[1] %in% c(0:59), rt_split[2] %in% c(0:59))

    return(paste0("00:00:", run_time))
  } else if (length(rt_split) == 1) {

    ## HH:MM:SS
    ## Check that seconds are between 0-59
    stopifnot(rt_split[1] %in% c(0:59))

    return(paste0("00:00:00:", run_time))
  } else {
    stop("Run-time is not in correct format. Exiting.")
  }
}
