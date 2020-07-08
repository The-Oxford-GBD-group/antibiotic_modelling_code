#################################################################################
### Saves post-estimation output files in the proper directory
## Inputs:
## Outputs:
#################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param filetype PARAM_DESCRIPTION
#' @param filename PARAM_DESCRIPTION
#' @param indic PARAM_DESCRIPTION, Default: indicator
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname save_post_est
#' @export
save_post_est <- function(x,
                          filetype,
                          filename,
                          indic = indicator) {
  output_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indic, "/output/", run_date)


  dir.create(output_dir, showWarnings = FALSE)

  filetype <- tolower(filetype)
  if (!filetype %in% c("rdata", "raster", "csv")) {
    stop("filetype argument has to be either rdata or raster or csv")
  }


  if (filetype == "raster") {
    writeRaster(
      x,
      file = paste0(output_dir, "/", indic, "_", filename),
      format = "GTiff",
      overwrite = TRUE
    )
  }

  if (filetype == "rdata") {
    save(
      x,
      file = paste0(output_dir, "/", indic, "_", filename, ".RData"),
      compress = TRUE
    )
  }

  if (filetype == "csv") {
    write.csv(
      x,
      file = paste0(output_dir, "/", indic, "_", filename, ".csv")
    )
  }
}
