#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param draws PARAM_DESCRIPTION
#' @param draw_prefix PARAM_DESCRIPTION
#' @param observed PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_stats_ho_id
#' @export
fit_stats_ho_id <- function(draws,
                            draw_prefix,
                            observed) {
  message("Your draws must contain columns oos and ho_id.")
  message("Summarizing draws called phat_*...")
  cols <- grep(draw_prefix, names(draws), value = TRUE)
  draws <- draws[, mean := .(mean = rowMeans(.SD)), by = ho_id, .SDcols = cols]
  draws <- draws[, upper := apply(.SD, 1, quantile, c(.975), na.rm = TRUE), .SDcols = cols]
  draws <- draws[, lower := apply(.SD, 1, quantile, c(.025), na.rm = TRUE), .SDcols = cols]

  message("Calculating fit statistics at each ho_id...")
  # Coverage
  draws <- draws[get(observed) >= lower & get(observed) <= upper, covered := 1]
  draws <- draws[get(observed) < lower | get(observed) > upper, covered := 0]

  # Error
  draws <- draws[, error := get(observed) - mean]

  # Make statistics over entire OOS dataset
  oos_mean_error <- mean(draws[oos == 1, error])
  oos_mean_absolute_error <- mean(draws[oos == 1, abs(error)])
  oos_rmse <- sqrt(mean(draws[oos == 1, error]^2))
  oos_coverage <- mean(draws[oos == 1, covered])

  message("OUT-OF-SAMPLE:")
  message(paste0("                       Average error:    ", round(oos_mean_error, 3)))
  message(paste0("                       Average MAE:      ", round(oos_mean_absolute_error, 3)))
  message(paste0("                       Average RMSE:     ", round(oos_rmse, 3)))
  message(paste0("                       Average coverage: ", round(oos_coverage, 3)))

  message("Returning draws with fit stat columns added.")
  return(draws)
}
