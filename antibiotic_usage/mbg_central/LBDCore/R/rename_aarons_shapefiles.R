#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param len PARAM_DESCRIPTION, Default: (
#' length(Regions) * 4
#' @param prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname rename_aarons_shapefiles
#' @export
rename_aarons_shapefiles <- function(len = (length(Regions) * 4), prefix) {
  # sneaky guy for roy :)
  for (i in 1:len)
    for (f in list.files(paste0(sharedir, "/output/", run_date, "/"), pattern = paste0("spat_holdout_stratum_", i, "_t")))
      file.rename(paste0(sharedir, "/output/", run_date, "/", f), paste0(sharedir, "/output/", run_date, "/", prefix, "_", gsub(paste0("spat_holdout_stratum_", i), names(stratum_qt)[[i]], f)))
}
