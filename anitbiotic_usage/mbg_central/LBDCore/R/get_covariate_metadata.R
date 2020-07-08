#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_covariate_metadata
#' @export
get_covariate_metadata <- function(list = NULL) {
  ## Where clause
  if (is.blank(list)) {
    where <- ""
  } else {
    where <- paste0("WHERE LOWER(covariate_name_short) in (", tolower(toString(shQuote(list))), ")")
  }

  ## Pull
  dbname <- "shared"
  host <- "modeling-covariates-db.ihme.washington.edu"
  query <- paste0("SELECT * FROM shared.covariate ", where)
  df <- run_query(dbname, host, query)

  ## Convert names to lowercase
  df$covariate_name_short <- tolower(df$covariate_name_short)

  ## Throw an error check if not int output
  if (!is.blank(list)) {
    return <- df$covariate_name_short
    if (length(list[which(!list %in% return)]) > 0) {
      stop(paste0("The following covariates do not exist in the db: ", toString(list[which(!list %in% return)])))
    }
  }
  return(df)
}
