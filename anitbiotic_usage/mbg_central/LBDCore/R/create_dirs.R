#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname create_dirs
#' @export
create_dirs <- function(indicator_group, indicator) {
  ## Create directory structure
  #   Arguments:
  #     indicator_group = Category of indicator, i.e. "education"
  #     indicator       = Specific outcome to be modeled within indicator category, i.e. "edu_0"

  dir.create(paste0("/share/geospatial/mbg/", indicator_group))
  dir.create(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator))

  indicator_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator)

  for (dir in c("output", "model_image_history")) {
    dir.create(paste0(indicator_dir, "/", dir), showWarnings = FALSE)
  }
}
