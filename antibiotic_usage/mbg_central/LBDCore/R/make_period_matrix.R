#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_period_matrix
#' @export
make_period_matrix <- function(period) {
  rf_shape <- merge(subset_shape, rf[rf$year == period, ], by = "GAUL_CODE")
  rf_shape@data$id <- rownames(rf_shape@data)
  rf.pts <- fortify(rf_shape, region = "id")
  rf.df <- join(rf.pts, rf_shape@data, by = "id")
  rf.df$raking_factor[is.na(rf.df$raking_factor)] <- 0
  rf.df$raking_factor <- as.numeric(rf.df$raking_factor)
  rf.df$year <- period
  return(rf.df)
}
