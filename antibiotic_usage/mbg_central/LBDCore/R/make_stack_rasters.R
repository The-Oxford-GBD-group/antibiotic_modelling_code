#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param covariate_layers PARAM_DESCRIPTION, Default: all_cov_layers
#' @param period PARAM_DESCRIPTION, Default: NULL
#' @param child_models PARAM_DESCRIPTION, Default: list()
#' @param stacker_model PARAM_DESCRIPTION, Default: stacked_results[[2]]
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param return_children PARAM_DESCRIPTION, Default: F
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param re PARAM_DESCRIPTION, Default: reg
#' @param ind_gp PARAM_DESCRIPTION, Default: indicator_group
#' @param ind PARAM_DESCRIPTION, Default: indicator
#' @param ho PARAM_DESCRIPTION, Default: holdout
#' @param centre_scale_df PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_stack_rasters
#'
#' @export
make_stack_rasters <- function(covariate_layers = all_cov_layers,
                               period = NULL,
                               child_models = list(),
                               indicator_family = "binomial",
                               rd = run_date,
                               re = reg,
                               ind_gp = indicator_group,
                               ind = indicator,
                               ho = holdout,
                               centre_scale_df = NULL,
                               ...) {

  ## first, save child_models for use in get.cov.wts in post_estiamtion if not other places too
  save(child_models, file = sprintf(
    "/share/geospatial/mbg/%s/%s/output/%s/child_model_list_%s_%s.RData",
    ind_gp, ind, rd, re, ho
  ))

  if (is.null(period)) {
    period <- 1:4
  }

  ## Create rasters from the child models
  res <- lapply(period, function(the_period) produce_stack_rasters(
      covariate_layers = covariate_layers, # raster layers and bricks. Covariate rasters essentially
      period = the_period, # period of analysis. Fed in from the lapply
      child_models = child_models, # a list of model objects for the child learners
      indicator_family = indicator_family,
      centre_scale_df = centre_scale_df
    ))


  ## Prep the rasters to be ordered in the following order:
  ## raster_brick[[child_models]][[period]]
  ret_obj <- lapply(names(child_models), function(x_cn) {
    raster::brick(lapply(period, function(x_t) res[[x_t]][[x_cn]]))
  })
  
  ## Fix for single period (rename with suffix ".1")
  if(length(period) == 1) {
    for(i in 1:length(ret_obj)) {
      names(ret_obj[[i]]) <- paste0(names(child_models)[i], ".1")
    }
  }
  
  ## Set names of the output raster list
  names(ret_obj) <- names(child_models)

  # Make sure that the raster brick dimensions and names are all correct
  stopifnot(assertthat::are_equal(length(ret_obj), length(names(child_models))))

  j <- 0
  for (x_t in ret_obj) {
    j <- j + 1
    stopifnot(assertthat::are_equal(names(x_t), paste0(names(child_models)[j], ".", period)))
  }


  # return!
  return(ret_obj)
}