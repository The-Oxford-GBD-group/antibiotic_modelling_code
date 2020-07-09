#' @title FUNCTION_TITLE
#' @description save a csv of contributions of different variables in brt
#' @param brt_mod_obj PARAM_DESCRIPTION, Default: trans_covs[[1]]
#' @param a PARAM_DESCRIPTION, Default: age
#' @param r PARAM_DESCRIPTION, Default: reg
#' @param pa PARAM_DESCRIPTION, Default: pathaddin
#' @param returnx PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname save_brt_contributions
#' @export
save_brt_contributions <- function(brt_mod_obj = trans_covs[[1]],
                                   a = age, r = reg, pa = pathaddin, returnx = FALSE) {
  x <- data.table(rbind(
    cbind(brt_mod_obj[[1]]$contributions, year = 2000, age = a, reg = r),
    cbind(brt_mod_obj[[2]]$contributions, year = 2005, age = a, reg = r),
    cbind(brt_mod_obj[[3]]$contributions, year = 2010, age = a, reg = r),
    cbind(brt_mod_obj[[4]]$contributions, year = 2015, age = a, reg = r)
  ))
  x$var <- gsub(pattern = "\\.[0-9]", replacement = "", x = x$var) # remove .# in varnames
  fwrite(x, paste0(
    "/share/geospatial/mbg/", indicator_group, "/",
    indicator, "/output/", run_date, "/",
    "brt_contribs_bin", pa, ".csv"
  ))
  if (returnx) return(x)
}
