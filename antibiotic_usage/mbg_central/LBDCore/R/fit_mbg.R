#' @title Fit MBG model in INLA
#' @description Fit MBG model in INLA
#' @param indicator_family PARAM_DESCRIPTION
#' @param stack.obs PARAM_DESCRIPTION
#' @param spde PARAM_DESCRIPTION
#' @param cov PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION
#' @param int_prior_mn PARAM_DESCRIPTION
#' @param int_prior_prec PARAM_DESCRIPTION, Default: 1
#' @param f_mbg PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param keep_inla_files PARAM_DESCRIPTION
#' @param cores PARAM_DESCRIPTION
#' @param verbose_output PARAM_DESCRIPTION, Default: FALSE
#' @param wgts PARAM_DESCRIPTION, Default: 0
#' @param intstrat PARAM_DESCRIPTION, Default: 'eb'
#' @param fe_mean_prior PARAM_DESCRIPTION, Default: 0
#' @param fe_sd_prior PARAM_DESCRIPTION, Default: 1
#' @param omp_start OpenMP strategy to be used, one of \code{c('small', 'medium', 'large', 'huge', 'default', 'pardiso.serial', 'pardiso.parallel')}.
#' Default: \code{'huge'}, which is the INLA default for TAUCS solver.
#' @param blas_cores Number of BLAS threads to use. Default: 1.
#' @param pardiso_license Path to PARDISO license. Must be provided if using a Pardiso startegy in \code{omp_strat}. Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_mbg
#' @export
fit_mbg <- function(indicator_family,
                    stack.obs,
                    spde,
                    cov,
                    N,
                    int_prior_mn,
                    int_prior_prec = 1,
                    f_mbg,
                    run_date,
                    keep_inla_files,
                    cores, 
                    verbose_output = FALSE,
                    wgts = 0,
                    intstrat = "eb",
                    fe_mean_prior = 0,
                    fe_sd_prior = 1,
                    omp_strat = "huge",
                    blas_cores = 1,
                    pardiso_license = NULL) {
  if (wgts[1] == 0) {
    wgts <- rep(1, length(N))
  }

  ## Assert that omp_strat is in the domains of what is possible
  stopifnot(omp_strat %in% c("small", "medium", "large", "huge", "default", "pardiso.serial", "pardiso.parallel"))


  message("Checking to see if PARDISO strategy is used")
  if (grepl("pardiso", omp_strat)) {

    ## Pardiso strategy NEEDS a license
    stopifnot(!is.null(pardiso_license))

    ## Set up and test PARDISO solver
    inla.setOption("pardiso.license", pardiso_license)
    (success_msg <- inla.pardiso.check())

    if (!grepl("SUCCESS", success_msg)) {
      warning("PARDISO solver was not set up properly. Reverting to 'huge' strategy")
      omp_strat <- "huge"
    }
  }


  # ~~~~~~~~~~~~~~~~
  # fit the model
  # enable weights
  inla.setOption("enable.inla.argument.weights", TRUE)

  message("Fitting INLA model")

  # set a prior variance of 1.96 on the intercept as this is
  # roughly as flat as possible on logit scale without >1 inflection

  # code just to fit the model (not predict)
  inla_working_dir <- paste0("/share/scratch/tmp/geos_inla_intermediate/inla_", run_date)
  dir.create(inla_working_dir, showWarnings = FALSE)
  if (indicator_family == "binomial") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          link = 1,
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          mean = list(int = int_prior_mn,
                      default = fe_mean_prior),
          prec = list(int = int_prior_prec,
                      default = fe_sd_prior)
        ),
        control.compute = list(
          dic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat
        ),
        control.inla = list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6),
        family = "binomial",
        num.threads = cores,
        blas.num.threads = blas_cores,
        Ntrials = N,
        verbose = verbose_output,
        working.directory = inla_working_dir,
        weights = wgts,
        keep = TRUE
      )
    )
  }
  if (indicator_family == "gaussian") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          prec.intercept = 1,
          mean.intercept = int_prior_mn,
          mean = 0,
          prec = 2
        ),
        control.compute = list(
          dic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat
        ),
        control.inla = list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6),
        family = "gaussian",
        num.threads = cores,
        blas.num.threads = blas_cores,
        verbose = TRUE,
        working.directory = inla_working_dir,
        weights = wgts,
        scale = N,
        keep = TRUE
      )
    )
  }
  if (indicator_family == "beta") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          prec.intercept = 1,
          mean.intercept = int_prior_mn,
          mean = 0,
          prec = 2
        ),
        control.compute = list(
          dic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat
        ),
        control.inla = list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6),
        family = "beta",
        num.threads = cores,
        blas.num.threads = blas_cores,
        verbose = TRUE,
        working.directory = inla_working_dir,
        weights = wgts,
        scale = N,
        keep = TRUE
      )
    )
  }

  return(res_fit)
}
