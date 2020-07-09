#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param i PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname group_wt_mean
#' @export
group_wt_mean <- function(i) {
  probs <- draw_matrix[, i]
  df_wt <- subset(df_year, select = c("bins", "exposure"))
  df_wt[, prob := probs]
  df_summ <- df_wt[, list(wmean_p_hat = weighted.mean(prob, w = exposure)),
    by = bins
  ]
  setnames(df_summ, "wmean_p_hat", paste0("wmean_p_hat_", i))
}
