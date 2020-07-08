#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fixed_effects PARAM_DESCRIPTION
#' @param positive_constrained_variables PARAM_DESCRIPTION, Default: NULL
#' @param interact_with_year PARAM_DESCRIPTION, Default: NULL
#' @param int PARAM_DESCRIPTION, Default: TRUE
#' @param nullmodel PARAM_DESCRIPTION, Default: FALSE
#' @param add_nugget PARAM_DESCRIPTION, Default: FALSE
#' @param add_ctry_res PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname build_mbg_formula
#' @note The \code{formula} object was created at the very end of the function after dealing with everything before that in strings.
#'
#' @export
build_mbg_formula <- function(fixed_effects,
                              positive_constrained_variables = NULL,
                              interact_with_year = NULL,
                              int = TRUE,
                              nullmodel = FALSE,
                              add_nugget = FALSE,
                              add_ctry_res = FALSE) {

  # Set up model equation
  if (int == TRUE) intercept <- "+int" else intercept <- ""
  f_null <- paste0("covered~-1", intercept)

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

  f_nugget <- "f(IID.ID, model = \"iid\", hyper = list(theta = list(prior = \"loggamma\", param = c(2, 1)))" ## loggamma with shape==2, inv.scale=1

  f_res <- "f(CTRY.ID, model = \"iid\", hyper = list(theta = list(prior = \"loggamma\", param = c(2, 1)))" ## loggamma with shape==2, inv.scale=1

  f_space <- "f(space, model = spde, group = space.group, control.group = list(model = \"ar1\")"


  if (!nullmodel) {
    f_lin <- fixed_effects
    f_mbg <- paste(f_null, f_lin, f_space, sep = "+")
    if (add_nugget == TRUE) f_mbg <- paste(f_null, f_lin, f_space, f_nugget, sep = "+")
    if (add_ctry_res == TRUE) f_mbg <- paste(f_null, f_lin, f_space, f_res, sep = "+")
    if (add_nugget == TRUE & add_ctry_res == TRUE) f_mbg <- paste(f_null, f_lin, f_space, f_nugget, f_res, sep = " + ")
  } else {
    f_mbg <- paste(f_null, f_space, sep = "+")
  }

  ## Finally, coerce to formula object from string
  print(as.formula(f_mbg))
  return(as.formula(f_mbg))
}
