#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param model_names PARAM_DESCRIPTION, Default: c("gam", "gbm")
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param bam PARAM_DESCRIPTION, Default: F
#' @param spline_args PARAM_DESCRIPTION, Default: list(bs = "ts", k = 3)
#' @param indicator PARAM_DESCRIPTION, Default: indicator
#' @param indicator_family PARAM_DESCRIPTION, Default: indicator_family
#' @param cores PARAM_DESCRIPTION, Default: 'auto'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname gam_stacker
#' @export
gam_stacker <- function(df, # the dataset in data table
                        model_names = c("gam", "gbm"),
                        weight_column = NULL, # prefixes of the models to be stacked
                        bam = F, # whether or not bam should be used
                        spline_args = list(bs = "ts", k = 3),
                        indicator = indicator, # the indicator of analysis
                        indicator_family = indicator_family,
                        cores = "auto") {

  # Stack models using GAM/BAM
  # df: the data frame
  # indicator: indicator of analysis
  # indicator_family: what is the analytical model of the family
  # bam: should bam be used?
  # spline_args: spline parameters passed to GAM/BAM function

  ## start function##

  # copy dataset to avoid weird data table scoping issues
  df <- copy(df)

  # format the outcome variable depending on the family
  if (indicator_family == "binomial") {
    df[, failures := N - get(indicator)] # failures in the sense they didn't get sick
    outcome <- df[, .(get(indicator), failures)]
    names(outcome)[1] <- indicator
  } else {
    outcome <- df[, .(get(indicator))]
    names(outcome)[1] <- indicator
  }

  outcome <- as.matrix(outcome)

  # format the child model results into the glm format
  # create new columns to hold the cv results, which are then replaced with the full results upon prediction
  df[, (model_names) := lapply(model_names, function(mn) get(paste0(mn, "_cv_pred")))]

  # Fit the gam as the stacker
  stacker <- fit_gam(
    df = df,
    covariates = paste(model_names, collapse = " + "),
    additional_terms = NULL,
    weight_column = NULL,
    bam = bam,
    spline_args = spline_args,
    auto_model_select = T,
    indicator = indicator,
    indicator_family = indicator_family,
    cores = cores
  )

  stacker$model_name <- "gam_stacker"

  # predict the results as fit from the crossvalidated stuff
  df[, stacked_cv_pred := predict(stacker, df, type = "response")]

  # overwrite the columns to work on the full fit child modules
  df[, (model_names) := lapply(model_names, function(mn) get(paste0(mn, "_full_pred")))]
  df[, stacked_pred := predict(stacker, df, type = "response")]

  # return the dataframe and the stacker model
  return(setNames(list(df[, stacked_pred], stacker), c("dataset", "stacker_model"))) # [,.(stacked_pred)]
}
