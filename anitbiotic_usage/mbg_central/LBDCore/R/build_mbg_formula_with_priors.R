#' @title Build an INLA formula for mbg
#' @description construct a formula object for use with R-INLA::inla() model fitting in conjunction with the prepped data object from build_mbg_data_stack()
#' @param fixed_effects PARAM_DESCRIPTION
#' @param positive_constrained_variables PARAM_DESCRIPTION, Default: NULL
#' @param interact_with_year PARAM_DESCRIPTION, Default: NULL
#' @param int PARAM_DESCRIPTION, Default: TRUE
#' @param nullmodel PARAM_DESCRIPTION, Default: FALSE
#' @param add_nugget PARAM_DESCRIPTION, Default: FALSE
#' @param nugget_prior PARAM_DESCRIPTION, Default: 'list(prior = 'loggamma', param = c(2, 1))'
#' @param add_ctry_res PARAM_DESCRIPTION, Default: FALSE
#' @param ctry_re_prior PARAM_DESCRIPTION, Default: 'list(prior = 'loggamma', param = c(2, 1))'
#' @param temporal_model_type PARAM_DESCRIPTION, Default: ''ar1''
#' @param temporal_model_theta_prior PARAM_DESCRIPTION, Default: 'list(prior = 'loggamma', param = c(1, 0.00005))'
#' @param temporal_model_theta1_prior PARAM_DESCRIPTION, Default: 'list(prior = 'normal', param = c(0, 1/(2.58^2)))'
#' @param no_gp PARAM_DESCRIPTION, Default: FALSE
#' @param stacker_names PARAM_DESCRIPTION, Default: child_model_names
#' @param coefs.sum1 PARAM_DESCRIPTION, Default: FALSE
#' @param subnat_RE Additional admin-1 random effect. Default: FALSE
#' @param use_space_only_gp Logical. include a space only (time stationary) gp. Default: FALSE
#' @param use_time_only_gmrf Logical. include a time only gp. Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[base]{strsplit}}
#' @rdname build_mbg_formula_with_priors
#' @note The \code{formula} object was created at the very end of the function after dealing with everything before that in strings.
#'
#' @export

build_mbg_formula_with_priors <- function(fixed_effects,
                                          positive_constrained_variables = NULL,
                                          interact_with_year = NULL,
                                          int = TRUE,
                                          nullmodel = FALSE,
                                          add_nugget = FALSE,
                                          nugget_prior = "list(prior = 'loggamma', param = c(2, 1))",
                                          add_ctry_res = FALSE,
                                          ctry_re_prior = "list(prior = 'loggamma', param = c(2, 1))",
                                          temporal_model_type = "'ar1'",
                                          temporal_model_theta_prior = "list(prior = 'loggamma', param = c(1, 0.00005))",
                                          temporal_model_theta1_prior = "list(prior = 'normal', param = c(0, 1/(2.58^2)))",
                                          no_gp = FALSE,
                                          stacker_names = child_model_names,
                                          coefs.sum1 = FALSE,
                                          subnat_RE = FALSE,
                                          subnat_re_prior = "list(prior = 'loggamma', param = c(1, 5e-5))",
                                          use_space_only_gp = FALSE,
                                          use_time_only_gmrf = FALSE,
                                          time_only_gmrf_type = "rw2") {
  if (nchar(stacker_names[1]) == 0 & coefs.sum1 == TRUE) {
    message("WARNING! You've chosen sum-to-1 but don't appear to be using any stackers. Unless you have a very good reason to do this, it probably doesn't make sense. As such, we're setting coefs.sum1 <- FALSE")
    coefs.sum1 <- FALSE
  }

  # Set up model equation
  if (int == TRUE) intercept <- "+int" else intercept <- ""
  f_null <- formula(paste0("covered~-1", intercept))

  if (!is.null(interact_with_year)) {
    for (iwy in interact_with_year) {
      fixed_effects <- gsub(iwy, paste0(iwy, "* factor(period)"), fixed_effects)
    }
    if (!is.null(positive_constrained_variables)) {
      stop("Cannot Both Constrain and Interact, one must be NULL.")
    }
  }

  if (!is.null(positive_constrained_variables)) {
    if (!all(positive_constrained_variables %in% strsplit(fixed_effects, " \\+ ")[[1]])) {
      stop("Not all your positive_constrained_variables match fixed_effects")
    }
    for (pcv in positive_constrained_variables) {
      v <- sprintf("f(%s, model='clinear',range=c(0,Inf),initial=0)", pcv)
      fixed_effects <- gsub(pcv, v, fixed_effects)
    }
  }

  f_nugget <- as.formula(paste0("~f(IID.ID, model = 'iid', hyper = list(theta=", nugget_prior, "))"))
  f_res <- as.formula(paste0("~f(CTRY.ID, model = 'iid', hyper = list(theta=", ctry_re_prior, "))"))
  f_subnat <- as.formula(paste0("~f(SUBNAT.ID, model = 'iid', constr=TRUE, hyper = list(theta=", subnat_re_prior, "))"))

  ## spatial gps correlated across grouping (usually time)
  test_rho_priors(temporal_model_theta1_prior) ## Report how priors for theta1 (Rho) are being used in Rho space.
  f_space_time <- as.formula(
    paste0(
      "~f(space, model = spde, group = space.group, control.group = list(model = ",
      temporal_model_type,
      ", ",
      "hyper = list(theta = ",
      temporal_model_theta_prior,
      ", theta1 = ",
      temporal_model_theta1_prior,
      ")))"
  ))
  ## space only gp
  f_space <- as.formula("~f(sp.no.t, model = spde.sp)")

  ## time only gp
  f_time <- as.formula(paste0("~f(t.no.sp, model = '", time_only_gmrf_type, "')"))
  f.e.v <- base::strsplit(all_fixed_effects, " + ", fixed = T)[[1]] ## fixed effect vector
  f_sum1 <- as.formula(paste0("~f(covar,
                              model = 'iid',
                              extraconstr = list(A = matrix(1, 1, ", length(f.e.v), "), e = 1),
                              hyper=list(theta=list(initial=log(inla.set.control.fixed.default()$prec),
                              fixed=TRUE)))"))

  if (nchar(fixed_effects) <= 1) { ## then we don't have any fixed effects, so run the nullmodel
    nullmodel <- TRUE
  }

  ## build formula starting with f_null

  f_mbg <- f_null

  if (!nullmodel) {
    if (coefs.sum1) {
      f_lin <- f_sum1 ## use the sum1 formula instead of fixed effects.
      # the'fixed' effects may now be
      # found @ inla$fit$summary.random
    } else {
      f_lin <- reformulate(fixed_effects)
    }

    f_mbg <- f_mbg + f_lin
  }

  if (!no_gp) f_mbg <- f_mbg + f_space_time
  if (use_space_only_gp) f_mbg <- f_mbg + f_space
  if (use_time_only_gmrf) f_mbg <- f_mbg + f_time
  if (add_nugget == TRUE) f_mbg <- f_mbg + f_nugget
  if (add_ctry_res == TRUE) f_mbg <- f_mbg + f_res
  if (subnat_RE == TRUE) f_mbg <- f_mbg + f_subnat

  message(f_mbg)
  return(f_mbg)
}
