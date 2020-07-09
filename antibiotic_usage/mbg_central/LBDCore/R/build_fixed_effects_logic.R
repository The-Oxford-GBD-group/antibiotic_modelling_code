#' @title Build Fixed Effects Logic for MBG
#'
#' @description Check variaous config parameters and determine the fixed effects
#' to be used in the MBG run
#'
#' @param dagobject The MBGDag object which contains the MBGPipeline object
#' along with the config parameters.
#' @param admin_codes A vector of GADM codes to be used if we want country
#' fixed effects in the MBG. Default: \code{NULL}
#'
#' @return A string with the formula for the fixed effects going into the MBG
#'
#' @export
build_fixed_effects_logic <- function(dagobject, admin_codes = NULL) {

  ## Take out the config parameters in our function scope
  ## which we'll build our logic on
  use_stacking_covs <- as.logical(dagobject$pipeline$config_list$use_stacking_covs)
  use_raw_covs <- as.logical(dagobject$pipeline$config_list$use_raw_covs)
  use_inla_country_fes <- as.logical(dagobject$pipeline$config_list$use_inla_country_fes)
  stacked_fixed_effects <- dagobject$pipeline$config_list$stacked_fixed_effects
  fixed_effects <- dagobject$pipeline$config_list$fixed_effects

  if (!use_stacking_covs & !use_raw_covs & !use_inla_country_fes) {
    all_fixed_effects <- ""
  }
  if (use_stacking_covs & !use_raw_covs & !use_inla_country_fes) {
    all_fixed_effects <- stacked_fixed_effects
  }
  if (!use_stacking_covs & use_raw_covs & !use_inla_country_fes) {
    all_fixed_effects <- fixed_effects ## from config
  }
  if (!use_stacking_covs & !use_raw_covs & use_inla_country_fes) {
    all_fixed_effects <- paste(
      names(admin_codes)[2:length(names(admin_codes))],
      collapse = " + "
    )
  }
  if (use_stacking_covs & use_raw_covs & !use_inla_country_fes) {
    all_fixed_effects <- paste(
      stacked_fixed_effects, fixed_effects,
      sep = " + "
    )
  }
  if (use_stacking_covs & !use_raw_covs & use_inla_country_fes) {
    all_fixed_effects <- paste(
      stacked_fixed_effects, paste(
        names(admin_codes)[2:length(names(admin_codes))],
        collapse = " + "
      ),
      sep = " + "
    )
  }
  if (!use_stacking_covs & use_raw_covs & use_inla_country_fes) {
    all_fixed_effects <- paste(
      fixed_effects, paste(
        names(admin_codes)[2:length(names(admin_codes))],
        collapse = " + "
      ),
      sep = " + "
    )
  }
  if (use_stacking_covs & use_raw_covs & use_inla_country_fes) {
    all_fixed_effects <- paste(
      stacked_fixed_effects, fixed_effects, paste(
        names(admin_codes)[2:length(names(admin_codes))],
        collapse = " + "
      ),
      sep = " + "
    )
  }

  return(all_fixed_effects)
}
