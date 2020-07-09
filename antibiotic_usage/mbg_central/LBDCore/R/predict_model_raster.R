#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param model_call PARAM_DESCRIPTION
#' @param covariate_layers PARAM_DESCRIPTION
#' @param constants PARAM_DESCRIPTION, Default: NULL
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param centre_scale_df PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname predict_model_raster
#'
#' @importFrom raster stack as.data.frame
#'
#' @export
predict_model_raster <- function(model_call,
                                 covariate_layers,
                                 constants = NULL,
                                 indicator_family = "binomial",
                                 centre_scale_df = NULL) {

  # predict model raster: given a model object and some covariates, predict out the results in raster form
  # model call: the model object you want to create rasters from
  # covariate_layers: a list of raster-like objects named identically to the models fit on the tabular data
  # constants: do any constants need to be fed to the prediction step?
  # indicator_family: model family. Specifies what sorts of transformations need doing
  # to do: add a transform switch

  message(paste0("predicting out:", model_call$model_name))
  # convert the raster objects into a named matrix
  dm <- raster::as.data.frame(raster::stack(covariate_layers), xy = T)

  # apply centre scaling
  if (!is.null(centre_scale_df)) {
    cs_dm <- centreScale(dm[, names(covariate_layers)], df = centre_scale_df)
    dm <- cbind(dm[, c("x", "y")], cs_dm)
  }

  orig_names <- names(dm)

  # add constants(if present) to the raster
  if (!is.null(constants)) {
    dm <- cbind(dm, constants)
  }
  # create a template
  dm$rid <- 1:nrow(dm)
  template <- dm[, c("rid", "x", "y")]

  # drop rows with NA data
  dm <- na.omit(dm)


  # if a gam or a bam
  # class(model_call)
  if (inherits(model_call, "gam") | inherits(model_call, "bam")) {
    ret_obj <- predict(model_call, newdata = dm, type = "response")
  } else if (inherits(model_call, "gbm")) {
    ret_obj <- predict(model_call, newdata = dm, n.trees = model_call$gbm.call$best.trees, type = "response")
  } else if (inherits(model_call, "glmnet")) {
    # glmnet is dumb and wants a matrix for new data


    dm_n <- names(dm)
    dm <- as.matrix(dm)
    colnames(dm) <- dm_n

    # predict(object, newx, s = NULL, type=c("link","response","coefficients","nonzero","class"), exact = FALSE, offset, ...)

    ret_obj <- predict(model_call, newx = data.matrix(dm[, rownames(model_call$beta)]), s = model_call$cv_1se_lambda, type = "link")

    # backtransform into probability/percentage space
    if (indicator_family == "binomial") {
      ret_obj <- invlogit(ret_obj)
    }


    # return dm to its data frame form
    dm <- data.frame(dm)
    colnames(ret_obj) <- "ret_obj"
  } else if (inherits(model_call, "randomForest")) {
    ret_obj <- predict(model_call, newdata = dm, type = "response")

    # back transform if binomial
    if (indicator_family == "binomial") {
      ret_obj <- invlogit(ret_obj)
    }
  } else if (inherits(model_call, "earth")) {
    ret_obj <- predict(model_call, newdata = dm, type = "response")
    colnames(ret_obj) <- "ret_obj"
  } else if (inherits(model_call, "train")) {
    # All caret objects use this framework
    ret_obj <- predict(model_call, newdata = dm)
  }

  # convert back to a raster

  # rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
  ret_obj <- cbind(data.frame(rid = dm[, c("rid")]), ret_obj = ret_obj)

  # restore to former glory
  ret_obj <- merge(template, ret_obj, by = "rid", all.x = T)
  setorder(ret_obj, rid)
  ret_obj <- rasterFromXYZ(ret_obj[, c("x", "y", "ret_obj")], res = res(covariate_layers[[1]]), crs = crs(covariate_layers[[1]]))

  # return the object
  return(setNames(ret_obj, model_call$model_name))
}
