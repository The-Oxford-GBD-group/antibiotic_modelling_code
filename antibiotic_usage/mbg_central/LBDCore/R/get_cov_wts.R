
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rd PARAM_DESCRIPTION
#' @param ind PARAM_DESCRIPTION
#' @param ind_gp PARAM_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param age PARAM_DESCRIPTION, Default: 0
#' @param holdout PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get.cov.wts
#' @export
get.cov.wts <- function(rd, ## run_date
                        ind, ## indicator
                        ind_gp, ## indicator_group
                        reg,
                        age = 0,
                        holdout = 0) {

  ## ############################################################
  ## ############################################################
  ## Find approximate raw covariate weights in final INLA model
  ##
  ## ############################################################
  ## ############################################################


  ## ##########################################
  ## load the workspaces and objects we need ##
  ## ##########################################

  pathaddin <- paste0("_bin", age, "_", reg, "_", holdout)

  this_config <- fread(paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/config.csv"))
  stacker_list <- this_config[V1 == "stacked_fixed_effects", V2]
  stackers_used <- strsplit(stacker_list, " ")
  stackers_used <- stackers_used[[1]][stackers_used[[1]] != "+"]

  ## load the data used to fit the stackers and reconstruct the design matrix
  load(paste0("/share/geospatial/mbg/", ind_gp, "/", ind, "/model_image_history/", rd, pathaddin, ".RData"))
  fit.data <- as.data.frame(df)
  fit.data <- fit.data[, -(1:max(match(stackers_used, colnames(fit.data))))] # this drops all the ID and post-stacking variables, leaving just the pre-stacking ones
  if ("gaul_code" %in% colnames(fit.data)) fit.data$gaul_code <- NULL
  if ("period" %in% colnames(fit.data)) fit.data$period <- NULL
  X <- fit.data
  rc <- colnames(X)

  ## load the stacker fits
  load(paste0(
    "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd,
    sprintf("/child_model_list_%s_%i.RData", reg, holdout)
  ))
  smo <- child_models
  rm(child_models)

  ## load the INLA/TMB fit
  load(paste0(
    "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd,
    sprintf("/%s_model_eb_bin%i_%s_%i.RData", ind, age, reg, holdout)
  ))
  fit <- res_fit
  rm(res_fit)

  ## make a matrix to hold the p-values/importances
  imp.mat <- as.data.frame(matrix(ncol = length(rc), nrow = length(smo), dimnames = list(names(smo), rc)))

  ## #####################################################################################
  ## now, for each of the models, add the pvalues to the corresponding rows and columns ##
  ## #####################################################################################

  ## ~~~~~
  ## gam ~
  ## ~~~~~
  gam <- smo[["gam"]]
  if (!is.null(gam) & class(gam)[1] != "try-error") {
    smoothed <- summary(gam)$s.table[, 4] ## smoothed table
    names(smoothed) <- substr(names(smoothed), 3, nchar(names(smoothed)) - 1)
    unsmoothed <- summary(gam)$p.table[, 4] ## parametric table
    all.p <- c(smoothed, unsmoothed)

    ## now match names and stick into our pval.matrix
    imp.mat["gam", ] <- all.p[rc]

    ##  convert from p-val to `importance`
    imp.mat["gam", ] <- -log(imp.mat["gam", ])
    imp.mat["gam", ][is.na(imp.mat["gam", ])] <- 0
    if (sum(is.infinite(unlist(imp.mat["gam", ]))) > 0) { # if a covariate has 'infinite' importance, assign it 1 and everything else 0
      imp.mat["gam", ][!is.infinite(unlist(imp.mat["gam", ]))] <- 0
      imp.mat["gam", ][is.infinite(unlist(imp.mat["gam", ]))] <- 1
    }
    imp.mat["gam", ] <- imp.mat["gam", ] / sum(imp.mat["gam", ])
  }

  ## ~~~~~
  ## gbm ~
  ## ~~~~~
  gbm <- smo[["gbm"]]
  if (!is.null(gbm) & class(gbm)[1] != "try-error") {
    rel.inf <- gbm$contributions
    imp.mat["gbm", ] <- rel.inf[rc, "rel.inf"]

    ## convert to scaled importance
    imp.mat["gbm", ] <- imp.mat["gbm", ] / sum(imp.mat["gbm", ])
  }

  ## all the penalized regression DO NOT give SD or p-vals...
  ## I 'standardize' the coefs as per a suggestion in this thread:
  ## https://stats.stackexchange.com/questions/14853/variable-importance-from-glmnet

  ## ~~~~~~~
  ## lasso ~
  ## ~~~~~~~
  lasso <- smo[["lasso"]]
  if (!is.null(lasso) & class(lasso)[1] != "try-error") {
    l <- lasso$cv_1se_lambda ## this is the CV lambda from the child fit
    l.idx <- which(lasso$lambda == l)

    sds <- apply(X, 2, sd, na.rm = T)
    unscaled.coefs <- lasso$beta[, l.idx]
    unscaled.coefs <- unscaled.coefs[rc]

    ## scaled
    scaled.coefs <- abs(unscaled.coefs) * sds

    ## put them in the matrix
    imp.mat["lasso", ] <- scaled.coefs[rc]

    ## convert to scaled importance
    imp.mat["lasso", ] <- imp.mat["lasso", ] / sum(imp.mat["lasso", ])
  }

  ## ~~~~~~~
  ## ridge ~
  ## ~~~~~~~
  ridge <- smo[["ridge"]]
  if (!is.null(ridge) & class(ridge)[1] != "try-error") {
    l <- ridge$cv_1se_lambda ## this is the CV lambda from the child fit
    l.idx <- which(ridge$lambda == l)

    sds <- apply(X, 2, sd, na.rm = T)
    unscaled.coefs <- ridge$beta[, l.idx]
    unscaled.coefs <- unscaled.coefs[rc]

    ## scaled
    scaled.coefs <- abs(unscaled.coefs) * sds

    ## put them in the matrix
    imp.mat["ridge", ] <- scaled.coefs[rc]

    ## convert to scaled importance
    imp.mat["ridge", ] <- imp.mat["ridge", ] / sum(imp.mat["ridge", ])
  }

  ## ~~~~~~
  ## enet ~
  ## ~~~~~~
  enet <- smo[["enet"]]
  if (!is.null(enet) & class(enet)[1] != "try-error") {
    l <- enet$cv_1se_lambda ## this is the CV lambda from the child fit
    l.idx <- which(enet$lambda == l)

    sds <- apply(X, 2, sd, na.rm = T)
    unscaled.coefs <- enet$beta[, l.idx]
    unscaled.coefs <- unscaled.coefs[rc]

    ## scaled
    scaled.coefs <- abs(unscaled.coefs) * sds

    ## put them in the matrix
    imp.mat["enet", ] <- scaled.coefs[rc]

    ## convert to scaled importance
    imp.mat["enet", ] <- imp.mat["enet", ] / sum(imp.mat["enet", ])
  }

  ## ~~~~~~~~~
  ## xgboost ~
  ## ~~~~~~~~~
  xgboost <- smo[["xgboost"]]
  if (!is.null(xgboost) & class(xgboost)[1] != "try-error") {
    load_R_packages("caret")
    # Extract row names to then assign to column names
    xg_names <- rownames(varImp(xgboost, scale = FALSE)$importance)

    # Extract coefficients, already correctly scaled
    scaled.coefs <- varImp(xgboost, scale = FALSE)$importance[[1]]

    # Assign column names
    names(scaled.coefs) <- xg_names

    # put them in the matrix
    imp.mat["xgboost", ] <- scaled.coefs[rc]
  }

  ## ##########################################
  ## Now we propagate through the INLA coefs ##
  ## ##########################################

  ## to account for different scaling in INLA we also create SDs of the covariates that go into INLA
  inla.X <- df[, paste0(names(smo), "_cv_pred"), with = F]
  if (this_config[V1 == "indicator_family", V2] == "binomial" & this_config[V1 == "stackers_in_transform_space", V2] == T) {
    inla.X <- logit(inla.X)
  }
  inla.sds <- apply(inla.X, 2, sd, na.rm = T)

  ## get the coefficients
  if ("sdrep" %in% names(fit)) { # TMB, w/ stackers as fixed effects
    inla.coefs <- fit$sdrep$par.fixed[names(fit$sdrep$par.fixed) == "alpha_j"]
    names(inla.coefs) <- fit$fenames
    inla.coefs <- inla.coefs[stackers_used]
  } else if ("covar" %in% names(fit$summary.random)) { # INLA, w/ stackers as random effects (to sum to one)
    inla.coefs <- fit$summary.random$covar$"mean"
    names(inla.coefs) <- stackers_used
  } else { # INLA, w/ stackers as fixed effects
    inla.coefs <- fit$summary.fixed[stackers_used, "mean"]
    names(inla.coefs) <- stackers_used
  }

  ## get the scaled coefficients
  inla.coefs <- inla.coefs[names(smo)]
  scaled.inla.coefs <- inla.coefs * inla.sds

  ## make and scale the INLA weighted relative importance and add as a row
  inla.rel.imp <- apply(imp.mat, 2, function(x) sum(x * scaled.inla.coefs))
  inla.rel.imp <- abs(inla.rel.imp)
  inla.rel.imp <- inla.rel.imp / sum(inla.rel.imp)
  imp.mat <- rbind(imp.mat, "INLA COMBINED" = inla.rel.imp)

  ## add the scaled coefs as a column
  imp.mat <- cbind(imp.mat, "SCALED.INLA.COEFS" = c(scaled.inla.coefs, NA))

  ## save to general output directory
  save(imp.mat,
    file = paste0(
      "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd,
      sprintf("/cov_wts_%s_holdout_%i.RData", reg, holdout)
    )
  )

  return(imp.mat)
}
