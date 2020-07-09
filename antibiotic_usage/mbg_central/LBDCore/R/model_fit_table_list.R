#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param regions PARAM_DESCRIPTION
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param holdout PARAM_DESCRIPTION, Default: 0
#' @param age PARAM_DESCRIPTION, Default: 0
#' @param ind PARAM_DESCRIPTION, Default: indicator
#' @param ind_gp PARAM_DESCRIPTION, Default: indicator_group
#' @param sharedir PARAM_DESCRIPTION, Default: sprintf("/share/geospatial/mbg/\%s/\%s", indicator_group, indicator)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname model_fit_table_list
#' @export
model_fit_table_list <- function(regions, rd = run_date, holdout = 0,
                                 age = 0,
                                 ind = indicator,
                                 ind_gp = indicator_group,
                                 sharedir = sprintf("/share/geospatial/mbg/%s/%s", indicator_group, indicator)) {


  ## #################################################################
  ## ~~~~~~~~~~~ make table of INLA model results ~~~~~~~~~~~~~~~~~ ##
  ## #################################################################
  ## takes in standard model run inputs outputs a table of fixed effect,
  ## spatio-temporal hyperparameter, and random effects parameter
  ## summaries.
  ## note: takes a little while since it has to recreate the
  ## SPDE INLA object since neither we nor INLA saved that object

  ## load models
  message(sprintf("Pulling together results for %s models", rd))

  tlist <- list()

  for (rr in regions) {
    message(sprintf("::on region %s", rr))
    reg <- rr

    message("::::loading in pre-INLA objects to get spde")
    pathaddin <- paste0("_bin", age, "_", rr, "_", holdout)
    load(paste0(
      "/share/geospatial/mbg/", ind_gp, "/",
      ind, "/model_image_history/", rd, pathaddin,
      ".RData"
    ))

    modnames <- c("gam", "gbm", "ridge", "enet", "lasso")

    full_raster_list <- cov_list

    ## hotfix!! inv.logit ## TODO ## TODO don't need this if either
    ##   1) resolve logit fits
    ##   2) have new runs where I didn't logit stackers
    for (mm in modnames) {
      if (min(na.omit(values(full_raster_list[[mm]][[1]]))) < 0) {
        message(sprintf("un-logiting: %s", mm))
        full_raster_list[[mm]] <- ilogit(full_raster_list[[mm]])
      }
    }

    ## for stacking, overwrite the columns matching the model_names so
    ## that we can trick inla into being our stacker
    df <- df[, paste0(child_model_names) := lapply(
      child_model_names,
      function(x) get(paste0(x, "_cv_pred"))
    )]

    ## Create SPDE INLA stack
    input_data <- build_mbg_data_stack(
      df = df,
      fixed_effects = all_fixed_effects,
      mesh_s = mesh_s,
      mesh_t = mesh_t,
      use_ctry_res = use_inla_country_res,
      use_nugget = use_inla_nugget
    )

    spde <- input_data[[2]]
    ## this is what we neede!

    message("::::loading in INLA fit\n")
    f <- sprintf(
      "%s/output/%s/inla_model_fit_pre_preds_%s_holdout_%i.RDS",
      sharedir, rd, reg, holdout
    )
    res_fit <- readRDS(f)

    ## now we extract what we need from the fit to get transformed spatial params
    res.field <- inla.spde2.result(res_fit, "space", spde, do.transf = TRUE)

    ## nominal range at 0.025, 0.5, 0.975 quantiles
    range <- inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.range.nominal[[1]])
    nom.var <- inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.variance.nominal[[1]])
    spat.hyps <- rbind(range, nom.var)
    rownames(spat.hyps) <- c("Nominal Range", "Nominal Variance")

    ## other hyperparmas
    hyps <- summary(res_fit)$hyperpar[-(1:2), ] ## first two rows are
    ## theta1, theta2 which
    ## we have in range and
    ## nom.var

    colnames(spat.hyps) <- colnames(hyps)[3:5]
    ## fixed effects
    fixed <- summary(res_fit)$fixed[, 1:6]

    ## combine them all and just keep three quantiles

    all.res <- rbind(
      fixed[, 3:5],
      spat.hyps,
      hyps[, 3:5]
    )
    tlist[[rr]] <- all.res
  }
  return(tlist)
}
