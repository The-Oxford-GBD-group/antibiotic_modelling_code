#' @title Produce Stack Rasters
#' @description Create a stacked prediction for each period and child model. Function returns a list of raster objects. First object is a brick of stacked results. Additional objects refer to the child models
#' @param covariate_layers list of raster objects that consititue the covs. Must share names with the DT that the models were fit on, Default: all_cov_layers
#' @param period  what period should be used. 1:N -- as of 11/2 we assume 4, Default: 1
#' @param child_models a list of model objects from the child models, Default: list()
#' @param stacker_model the model object from the stacker, Default: stacker
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param return_children PARAM_DESCRIPTION, Default: F
#' @param centre_scale_df PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname produce_stack_rasters

#' @export
produce_stack_rasters <- function(covariate_layers = all_cov_layers, # raster layers and bricks
                                  period = 1, # period of analysis
                                  child_models = list(),
                                  indicator_family = "binomial",
                                  centre_scale_df = NULL) {
  message(paste0("The Period is ", period))

  # fetch the covariates appropriate for the period
  period_covs <- brick(lapply(covariate_layers, function(x) fetch_covariate_layer(x, period)))

  # create constants -- only flexible for year/period
  year <- data.frame(year = (1995 + period * 5))

  # predict the various models. This is super strict with variable names (and beware scoping issues with the names)
  # brick the results
  stacker_predictors <- brick(lapply(child_models, function(x) predict_model_raster(x,
      period_covs,
      constants = year,
      indicator_family = indicator_family,
      centre_scale_df = centre_scale_df
    )))


  return(stacker_predictors)
}
