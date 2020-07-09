#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param var PARAM_DESCRIPTION
#' @param by_vars PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname collapse_by
#' @export
collapse_by <- function(df, var, by_vars, ...) {

  ## Subset to frame where data isn't missing
  df.c <- copy(df[!is.na(get(var)) & !is.na(strata) & !is.na(psu) & !is.na(pweight)])

  ## Setup design
  design <- setup_design(df.c, var)

  ## Setup by the by call as a formula
  by_formula <- as.formula(paste0("~", paste(by_vars, collapse = "+")))

  ## Calculate mean and standard error by by_var(s).  Design effect is dependent on the scaling of the sampling weights
  est <- svyby(~ get(var), by_formula, svymean, design = design, deff = "replace", na.rm = TRUE, drop.empty.groups = TRUE, keep.names = FALSE, multicore = TRUE, ...)
  setnames(est, c("get(var)", "DEff.get(var)"), c("mean", "deff"))

  ## Calculate number of observations, number of clusters, strata
  meta <- df.c[, list(
    ss = length(which(!is.na(get(var)))),
    nclust = length(unique(psu)),
    nstrata = length(unique(strata)),
    var = var
  ), by = by_vars]

  ## Combine meta with est
  out <- merge(est, meta, by = by_vars)

  return(out)
}
