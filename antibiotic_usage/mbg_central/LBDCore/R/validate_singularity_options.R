#' @title Validate Singularity options
#'
#' @description Validates singularity-related options and errors if invalid.
#'
#' @param singularity the string name of the singularity image to use.
#'
#' @param singularity_opts named list of options.
#'
#' @export
validate_singularity_options <- function(singularity, singularity_opts) {
  if (is.null(singularity) & !is.null(singularity_opts)) {
    message("'singularity' argument is 'NULL' but options passed in for 'singularity_opts'")
    stop("Exiting!")
  }
}
