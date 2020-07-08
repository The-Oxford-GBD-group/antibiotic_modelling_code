#' @title Returns newest worldpop measure release
#' @description Checks the existing \code{config} object to determine which worldpop measure to check, and returns thew newest release for that measure.
#'
#' @param config a \code{data.table} with the existing configuration. This MUST have two columns named V1 and V2. V1 should contain configuration variable names one of which is "pop_measure". The V2 should contain configuration values e.g., "a0004t".
#' @export
#' @return character for the newest release e.g., "2017_04_27"
get_newest_worldpop_release <- function(config) {
  pop_measure <- config[V1 == "pop_measure", V2]
  helper <- CovariatePathHelper$new()
  measure.path <- helper$covariate_paths(covariates = c("worldpop"), measures = c(pop_measure))
  newest.release <- helper$newest_covariate_release(measure.path[1])
  return(newest.release)
}
