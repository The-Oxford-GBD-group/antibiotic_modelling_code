#' @title Reads covariate configuration file.
#' @description Read covariate configuration file providing sensible default values \code{header = TRUE, fill = TRUE}
#' @param path_or_text the file path or literal CSV text to read in.
#' @param ... any additional arguments to pass to \code{data.table::fread}
#' @return configuration as a data.table
#' @rdname read_covariate_config
#' @export
read_covariate_config <- function(path_or_text, ...) {
  data.table::fread(path_or_text,
    fill = TRUE, # fill blank fields in rows with uneven length
    header = TRUE, # first line is a header
    ...
  )
}
