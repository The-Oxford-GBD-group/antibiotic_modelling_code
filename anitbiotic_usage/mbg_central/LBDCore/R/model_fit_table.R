#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lv PARAM_DESCRIPTION, Default: loopvars[loopvars[, 3] == 0, ]
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param nullmodel PARAM_DESCRIPTION, Default: ''
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_group PARAM_DESCRIPTION, Default: indicator_group
#' @param holdout PARAM_DESCRIPTION, Default: 0
#' @param coefs.sum1 PARAM_DESCRIPTION
#' @param use_gp PARAM_DESCRIPTION, Default: use_gp
#' @param spde_prior PARAM_DESCRIPTION, Default: spde_prior
#' @param use_stacking_covs PARAM_DESCRIPTION, Default: use_stacking_covs
#' @param stacker_name_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[INLA]{character(0)}}
#' @rdname model_fit_table
#' @export
model_fit_table <- function(lv = loopvars[loopvars[, 3] == 0, ],
                            rd = run_date,
                            nullmodel = "",
                            indicator = indicator,
                            indicator_group = indicator_group,
                            holdout = 0,
                            coefs.sum1,
                            use_gp = use_gp,
                            spde_prior = spde_prior,
                            use_stacking_covs = use_stacking_covs,
                            stacker_name_vec) {

  ## ~~~~~~~~~~~ make table of model results ~~~~~~~~~~~~~~~~~

  # load models
  message(sprintf("Pulling together table for %s models", rd))
  tlist <- list()
  sharedir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", rd, "/")
  for (i in 1:nrow(lv)) {
    reg <- lv[i, 1]
    age <- lv[i, 2]
    message(sprintf("%i of %i loopvars. %s %i", i, nrow(lv), lv[i, 1], lv[i, 2]))

    # Recreate the INLA data stack
    pathaddin <- paste0("_bin", age, "_", reg, "_", holdout)
    f <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", rd, pathaddin, ".RData")

    if (!file.exists(f)) {
      message("FAILED TO OPEN")
    } else {
      load(f)
    }

    stacker_name_vec <- intersect(stacker_name_vec, colnames(df))
    if (length(stacker_name_vec) > 0) {
      df <- df[, paste0(stacker_name_vec) := lapply(
        stacker_name_vec,
        function(x) get(paste0(x, "_cv_pred"))
      )]
    }

    # Get the spde
    if (grepl("geos", Sys.info()[4])) INLA:::inla.dynload.workaround()
    input_data <- build_mbg_data_stack(
      df = df,
      fixed_effects = all_fixed_effects,
      mesh_s = mesh_s,
      mesh_t = mesh_t,
      spde_prior = spde_prior,
      use_ctry_res = FALSE,
      use_nugget = FALSE,
      stacker_names = stacker_name_vec,
      exclude_cs = stacker_name_vec
    )

    spde <- input_data[[2]]

    # Now get & transform model fit
    message("::::loading in INLA fit\n")
    f <- paste0(sharedir, indicator, "_model_eb_bin", age, "_", reg, "_0.RData")

    if (!file.exists(f)) {
      message("FAILED TO OPEN")
    } else {
      load(f)
    }

    ## columns we'll show return
    keep.cols <- c("0.025quant", "0.5quant", "0.975quant")

    ## other hyperparmas
    hyps <- summary(res_fit)$hyperpar[-(1:2), keep.cols] ## first two rows are
    ## theta1, theta2 which
    ## we have in range and
    ## nom.var

    if (as.logical(use_gp)) {
      if(eval(parse(text=spde_prior))$type=="pc") {
        ## extract values from the fit directly
        range <- res_fit$summary.hyperpar[1,keep.cols]
        nom.var <- res_fit$summary.hyperpar[2,keep.cols]^2
      } else {
        ## now we extract what we need from the fit to get transformed spatial params
        res.field <- INLA::inla.spde2.result(res_fit, 'space', spde, do.transf=TRUE)
        
        ## nominal range at 0.025, 0.5, 0.975 quantiles
        range   <- INLA::inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.range.nominal[[1]])
        nom.var <- INLA::inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.variance.nominal[[1]])
      }
      spat.hyps <- rbind(range, nom.var)
      rownames(spat.hyps) <- c("Nominal Range", "Nominal Variance")
      colnames(spat.hyps) <- keep.cols
    }

    ## fixed effects from coefs.sum1
    if (as.logical(coefs.sum1) & as.logical(use_stacking_covs)) {
      fixed.sum1 <- res_fit$summary.random$covar
      fixed.sum1$ID <- NULL
      rownames(fixed.sum1) <- stacker_name_vec
      fixed.sum1 <- fixed.sum1[, keep.cols]
    } else {
      fixed.sum1 <- NULL
    }

    ## all other coefs (e.g. intercept and raw covs)
    fixed <- summary(res_fit)$fixed
    if (is.null(nrow(fixed))) {
      fixed <- matrix(fixed, ncol = length(fixed)) ## in the event of one row, convert numeric back to data.frame
      rownames(fixed) <- rownames(summary(res_fit)$fixed)
      colnames(fixed) <- colnames(summary(res_fit)$fixed)
    }
    fixed <- fixed[, keep.cols]

    ## combine the two types of 'fixed' results
    fixed <- rbind(fixed, fixed.sum1)

    ## combine them all and just keep three quantiles
    all.res <- rbind(
      fixed,
      if (use_gp) {
        spat.hyps
      } else {
        NULL
      },
      hyps
    )

    ## rename GPRandom rho for time
    all.res <- as.data.table(all.res, keep.rownames = T)
    setnames(all.res, "rn", "parameter")
    if (use_gp) all.res[parameter == "GroupRho for space", parameter := "GPRandom rho for time"]
    all.res

    tlist[[paste0(reg, "_", age)]] <- all.res
  }

  return(tlist)
}
