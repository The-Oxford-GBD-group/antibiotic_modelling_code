#' @title Easy eval-parse
#' @description Allows for easily eval-parsing through a config dataset
#' @param data The data.table
#' @param column The column with the string call
#' @return Evaluated call
#' @export
#' @rdname ez_evparse
ez_evparse <- function(data, column) {
  return(eval(parse(text = data[, column, with = FALSE])))
}
