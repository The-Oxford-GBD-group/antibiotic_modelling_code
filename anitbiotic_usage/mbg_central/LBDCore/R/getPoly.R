#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param level PARAM_DESCRIPTION
#' @param code PARAM_DESCRIPTION
#' @param admin1 PARAM_DESCRIPTION
#' @param admin2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getPoly
#' @export
getPoly <- function(level, code, admin1, admin2) {

  # given the admin level, a GAUL code and the admin1 and 2 shapefiles,
  # return an SPDF with the relevant area

  # get admin level
  if (level == 1) {
    admin <- admin1
  } else {
    admin <- admin2
  }

  # find the reight area
  idx <- match(code, admin$GAUL_CODE)

  # if it's valid
  if (length(idx) == 1) {
    ans <- admin[idx, ]
  } else {
    # handle errors
    warning(paste0("something's wrong on row ", i))
    ans <- NULL
  }

  # return result
  return(ans)
}
