#' @title rank_draws
#' @description function to transform values to ranks by draw
#' @param df data frame with columns for each draw
#' @param 'high' means a high value is rank 1, 'low' means a
#' low value should be rank 1
#' @param columns: vector of column names that contain the draws
#' @export

rank_draws <- function(df, ordr, columns) {

  # Ensure data.table class
  df <- as.data.table(df)
  # Separate into df with draws and the rest
  df1 <- df[, setdiff(names(df), columns), with = FALSE]
  df2 <- df[, columns, with = FALSE]

  # Transform draws to ranks
  if (ordr == "high") {
    descending <- TRUE
  } else {
    descending <- FALSE
  }
  df2 <- as.data.table(apply(df2, 2, order, decreasing = descending))

  # Summarize ranks
  df2 <- df2[, `:=`(
    median = apply(df2, 1, median),
    lci = apply(df2, 1, quantile, 0.025),
    uci = apply(df2, 1, quantile, 0.975)
  )]

  # Bind to make a single df
  df <- cbind(df1, df2)
  return(df)
}
