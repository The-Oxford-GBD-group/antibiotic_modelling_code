#' @title Update fixed effect covariate configuration with optional/missing data
#' @description Updates the fixed effect configuration with \emph{release} values for each
#' covariate/measure pair if it is not provided by using the most recent available release.
#' @param fixed_effect_config the fixed effect configuration, a data.table
#' @return NULL (the fixed_effect_config is modified in-place)
#' @rdname update_fixed_effect_config_with_missing_release
#' @export
update_fixed_effect_config_with_missing_release <- function(fixed_effect_config) {
  # add "release" column if not present.
  # fill it with the default fill value of data.table::fread
  if (!"release" %in% names(fixed_effect_config)) {
    # use explicit NA type so subsequent updating with character works
    # https://stackoverflow.com/a/15554528
    fixed_effect_config[, release := NA_character_]
    indices.to.update <- 1:nrow(fixed_effect_config)
  } else {
    indices.to.update <- which(fixed_effect_config$release == "")
  }
  if (length(indices.to.update) == 0) {
    return(NULL)
  }
  helper <- CovariatePathHelper$new()
  covariate.paths <- helper$covariate_paths(
    covariates = fixed_effect_config[indices.to.update, covariate],
    measures = fixed_effect_config[indices.to.update, measure])
  releases <- helper$newest_covariate_release(covariate.paths)
  fixed_effect_config[indices.to.update, release := releases]
  return(NULL)
}
