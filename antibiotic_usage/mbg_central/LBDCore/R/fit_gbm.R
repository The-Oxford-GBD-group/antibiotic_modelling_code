#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: all_fixed_effects
#' @param additional_terms PARAM_DESCRIPTION, Default: NULL
#' @param weight_column PARAM_DESCRIPTION, Default: NULL
#' @param tc PARAM_DESCRIPTION, Default: 4
#' @param lr PARAM_DESCRIPTION, Default: 0.005
#' @param bf PARAM_DESCRIPTION, Default: 0.75
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_family PARAM_DESCRIPTION, Default: 'binomial'
#' @param plot.main PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname fit_gbm
#' @export
#' @importFrom dismo gbm.step
fit_gbm <- function(df, covariates = all_fixed_effects, additional_terms = NULL, weight_column = NULL, tc = 4, lr = .005, bf = .75, indicator, indicator_family = "binomial",
                    plot.main = F) {

  # adapted from Roy's BRT covs function-- took away the year specific bit. GBMs==BRTs
  # df: a data table (post extract covariates)
  # covariates: rhs of formula specifying the covariates/columns to be used to help fit the model
  # weight_column: column in the data table that specifies the weight
  # tc: tree complexity
  # lr: learning rate
  # bf: bag fraction
  # indicator: dependant variable.
  # indicator_family: Binomial models assume N as the # of trials and are actually modelled with poission with N as the offset



  # check to see if its a vector of characters or a psudo-formula
  covariates <- format_covariates(add_additional_terms(covariates, additional_terms))

  df <- copy(df)

  # format weights
  if (!is.null(weight_column)) {
    df[, data_weight := get(weight_column)]
  } else {
    df[, data_weight := 1]
  }
  weight_column <- "data_weight" # specify the column


  # BRT function we use has no binomial. Use emperical logistic or poisson

  # set up poisson outcome structure for binomial and poisson data
  if (indicator_family %in% c("binomial", "poisson")) {
    indicator_family <- "poisson"
    offset <- log(df[, N])
    df[, pre_round := get(indicator)]
    # message('WARNING: For Poisson to work, need to round decimals in the response')
    df[, paste0(indicator) := round(pre_round, 0)] # round indicator to 0
  } else {
    offset <- NULL
  }

  # run the brts. The logic is similiar to the BRT covs (e.g. copy pasted). NOTE, this will run a model for all periods at once. Brt_covs runs each year independantly.
  # learning brt

  message(paste("Fitting GBM/BRT with tc:", tc, "lr:", lr, "bf:", bf))

  # TODO: throw a try-catch so if some years work it at least will return that, if it doesnt it will try different things (like changing the learning rate. )
  mod <- try(
    dismo::gbm.step(
      data = as.data.frame(df),
      gbm.y = indicator,
      gbm.x = covariates,
      offset = offset,
      family = indicator_family,
      site.weights = df[, get(weight_column)],
      tree.complexity = tc,
      learning.rate = lr,
      bag.fraction = bf,
      silent = T,
      plot.main = F,
      plot.folds = F
    ),
    silent = TRUE
  )

  if (is.null(mod)) {
    message("First BRT attempt failed. Lowering Learning Rate by 1/10")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = tc,
        learning.rate = lr * .1,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }
  if (is.null(mod)) {
    message("Second BRT attempt failed. Lowering Original Learning rate by 1/1000 AGAIN")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = tc,
        learning.rate = lr * .001,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }
  if (is.null(mod)) {
    message("Third BRT attempt failed. Slow learn plus low tree complexity")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = 2,
        learning.rate = lr * .001,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }

  if (is.null(mod)) stop("ALL BRT ATTEMPTS FAILED")

  return(mod)
}
