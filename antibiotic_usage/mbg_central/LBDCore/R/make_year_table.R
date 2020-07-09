#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param this_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_year_table
#' @export
make_year_table <- function(this_year) {
  df_year <- df[bin_year == this_year, ]

  # Create bins by predicted probability for that year
  df_year$bins <- cut2(df_year[, get(sample)], cuts = seq(0, 1, 0.1))
  levels(df_year$bins) <- c(
    "[0.0, 0.1)",
    "[0.1, 0.2)",
    "[0.2, 0.3)",
    "[0.3, 0.4)",
    "[0.4, 0.5)",
    "[0.5, 0.6)",
    "[0.6, 0.7)",
    "[0.7, 0.8)",
    "[0.8, 0.9)",
    "[0.9, 1.0]"
  )

  # calculate summary measures (weighted means) for each bin
  df_year_summ <- df_year[, list(
    wmean_pred = weighted.mean(get(sample), w = exposure),
    wmean_data = weighted.mean(hit_target, w = exposure),
    N_total = sum(N * weight)
  ),
  by = bins
  ]

  # now simulate using our predicted probability and the number of observations
  # try to capture uncertainty in sample sizes

  message(paste0(">> Simulating for year ", this_year, "..."))
  n_draws <- 500

  # simulate draws using N for each cluster and predicted probability
  # each cell represents the observed probability for a simulation
  # given N for the cluster and assuming that the predicted value is true

  draw_matrix <- mapply(function(draws, N, prob) rbinom(draws, N, prob) / N,
    draws = n_draws,
    N = df_year$N,
    prob = df_year[, get(sample)]
  ) %>% t()


  # Create summary table (by draws) of weighted means by bin
  df_draw_summ <- lapply(1:ncol(draw_matrix), group_wt_mean) %>%
    Reduce(merge, .)

  draw_cols <- names(df_draw_summ)[grepl("wmean_p_hat", names(df_draw_summ))]

  # generate 5th, 50th, and 95th %iles
  df_draw_summ <- cbind(
    subset(df_draw_summ, select = "bins"),
    apply(subset(df_draw_summ, select = draw_cols),
      1,
      quantile,
      probs = c(0.05, 0.5, 0.95)
    ) %>%
      t() %>%
      as.data.table()
  )

  df_summ <- merge(df_year_summ, df_draw_summ, by = "bins")
  df_summ[, year := this_year]
}
