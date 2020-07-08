#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @note   Adapted by Jon Mosser from Roy Burstein's `clean_model_results()` function
#' @param rd PARAM_DESCRIPTION, Default: run_date
#' @param regs PARAM_DESCRIPTION, Default: Regions
#' @param ages PARAM_DESCRIPTION, Default: 0
#' @param nm PARAM_DESCRIPTION, Default: ''
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @param ig PARAM_DESCRIPTION, Default: indicator_group
#' @param stackers PARAM_DESCRIPTION, Default: stacked_fixed_effects
#' @param coefs.sum1 PARAM_DESCRIPTION, Default: as.logical(coefs_sum1)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname clean_model_results_table
#' @export
#' @importFrom stringr str_match
clean_model_results_table <- function(rd = run_date,
                                      regs = Regions,
                                      ages = 0,
                                      nm = "",
                                      indic = indicator,
                                      ig = indicator_group,
                                      stackers = stacked_fixed_effects,
                                      coefs.sum1 = as.logical(coefs_sum1)) {


  sharedir <- paste0("/share/geospatial/mbg/", ig, "/", indic, "/output/", rd, "/")

  # make loopvars
  lv <- expand.grid(regs, ages)

  # grab formatted model fit objects
  stacker_names <- strsplit(stackers, " + ", fixed = T)[[1]]
  mods <- model_fit_table(
    lv = lv, rd = rd, nullmodel = nm, indicator = indic, indicator_group = ig,
    coefs.sum1 = coefs.sum1, stacker_name_vec = stacker_names,
    use_stacking_covs = use_stacking_covs, use_gp = use_gp, spde_prior = spde_prior
  )

  # add region column
  all_mods <- lapply(names(mods), function(n) {
    mod <- data.table(mods[[n]])

    r <- stringr::str_match(n, "(.*)_")[1, 2]
    a <- stringr::str_match(n, "_(.*)")[1, 2]
    mod[, region := r]
    mod[, age := a]
    return(mod)
  })

  all_mods <- rbindlist(all_mods)
  colorder <- c(
    "region", "age",
    names(all_mods)[!(names(all_mods) %in% c("region", "age"))]
  )
  setcolorder(all_mods, colorder)

  write.csv(all_mods, paste0(sharedir, indic, "_model_results_table.csv"),
    row.names = F
  )

  return(mods)
}
