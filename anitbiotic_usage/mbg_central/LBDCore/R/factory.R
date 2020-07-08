
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fun PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname factory
#' @export
factory <- function(fun)
                    # Additional error handling functions from
#   https://stackoverflow.com/questions/4948361/
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }),
      warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(res, warn = warn, err = err)
  }
