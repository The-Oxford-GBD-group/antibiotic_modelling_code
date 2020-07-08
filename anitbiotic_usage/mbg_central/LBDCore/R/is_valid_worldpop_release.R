#' @title Tests if worldpop release is valid.
#' @param ignore_me_or_get_error an unused parameter
#' @details ignore_me_or_get_error parameter provided by ez_evparse() from set_up_config. DO NOT
#'  try to use this parameter in any way or R will crash with an "unxpected input" error because the string cannot
#'  be evaluated into anything (unlike e.g., "142" which can be evaluated into the numeric 142)
is_valid_worldpop_release <- function(ignore_me_or_get_error) {
  # this function requires two arguments to run correctly: a worldpop measure and a worldpop release.
  # due to how the configuration testing works neither of these values are available to our function,
  # but they are available in the environment of the calling context.
  #
  # as a terrible-but-functional work-around we simply extract those values from the calling function's environment
  # https://stackoverflow.com/a/17830210
  parent.env <- parent.frame(n = 1)
  config <- parent.env$config
  if (is.null(config)) {
    stop("Cannot access 'config' object - cannot retrieve pop_measure and pop_release")
  }

  worldpop_measure <- config[V1 == "pop_measure", V2]
  if (length(worldpop_measure) == 0) {
    stop("Cannot access pop_measure, so cannot validate pop_release")
  }

  release <- config[V1 == "pop_release", V2]
  if (length(release) == 0) {
    stop("Cannot access pop_release, so cannot validate pop_release")
  }

  helper <- CovariatePathHelper$new()
  release.path <- helper$covariate_paths(
    covariates = c("worldpop"),
    measures = c(worldpop_measure),
    releases = c(release)
  )[1]
  if (!dir.exists(release.path)) {
    stop(sprintf("worldpop measure %s does not have a release %s", worldpop_measure, release))
  }
}
