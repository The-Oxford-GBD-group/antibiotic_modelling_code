#####################################################################################################################################################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df1 PARAM_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname age_sex_merge
#' @export
age_sex_merge <- function(df1, df2) {

  ## Find specificity
  spec1 <- age_sex_spec(df1)
  spec2 <- age_sex_spec(df2)
  test <- data.table(spec1 == spec2)

  ## Merge
  cols <- c("location_id", "year_id", "age_group_id", "sex_id")
  drop_cols <- NULL
  merge_cols <- col

  ## If age and sex match
  if (test$by_age & test$by_sex) {
    df <- merge(df1, df2, by = cols)
  } else {
    ## If age matches but sex doesn't match
    if (test$by_age & !test$by_sex) {
      drop_cols <- "sex_id"
    }
    ## If age doesnt match and sex matches
    else if (!test$by_age & test$by_sex) {
      drop_cols <- "age_group_id"
    }
    ## If neither match
    else {
      drop_cols <- c("sex_id", "age_group_id")
    }
    ## Merge
    merge_cols <- cols[!cols %in% drop_cols]
    df <- merge(df1, df2[, drop_cols := NULL, with = F], by = merge_cols)
  }
  return(df)
}
