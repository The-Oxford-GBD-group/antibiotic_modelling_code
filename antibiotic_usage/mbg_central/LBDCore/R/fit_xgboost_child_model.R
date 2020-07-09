#' @title Fit xgboost child model function
#' @description This function fits xgboost, another implementation of boosted regression trees. Xgboost has a different fitting algorithm
#' as compared to gbm, and tends to fit much faster with fewer number of iterations required. This speeds up the run time considerably.
#'
#' This function takes a data frame with the extracted covariates at each lat/long and fits regression trees on your indicator.
#' The added bonus of xgboost for prevalence indicators is it can perform logistic regression with an outcome variable between
#' \code{[0,1]}, so we no longer need to convert brt's to a poisson distribution.
#'
#' This function also has built in cross-validation that uses 5 fold cross validation repeated 5 times for model accuracy.
#' You can either pass the hyperparameter space as a file path to a csv file, or go with the default grid search by not providing a filepath.
#' Xgboost has a large number of parameters, but as of now the only 3 tunable hyperparameters. The number of rounds (nround) specifies the number of iterations.
#' Eta (learning rate) which specifies the shrinkage used in updating each tree fit to prevent overfitting. After each boosting step, we can
#' directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative. Finally, we also vary
#' the max depth, which specifies the maximum depth of the tree. Increasing this value will make the model more complex and more likely to overfit.
#'
#'
#' @author Michael Cork, \email{mcork23\@uw.edu}
#'
#' @param df Data Frame. Data frame that contains your indicator and the covariate values extracted at those cluster locations
#' @param indicator Character. Model indicator. Make sure in your data frame that this exists
#' @param indicator_family Character. Indicator statistical family. Common inputs include Binomial and Gaussian
#' @param outputdir File path. Model output directory, defined early on in the parallel script
#' @param region Character. Region being modeled over
#' @param covariates Character. List of covariates being used in stacking. For example: access2 + aridity + fertility
#' @param weight_column Numeric. This column contains the weights form polygon resampling.
#' @param xg_model_tune Logical. If true, xgboost will perform grid search to pick best hyperparameters
#' @param hyperparameter_filepath File path. If true, xgboost will perform a grid search to find the optimal hyperparameter.
#'                                If false, it will read from config what the optimal parameters. You must specify nrounds, eta and max depth
#'
#' @return a list containing: 1) The out of sample and in sample predictions from the xgboost fit, labeled "dataset"
#'                            2) The child model object, labeled "xgboost"
#'
#' @importFrom caret trainControl train
#' @importFrom raster extent
#' @export
fit_xgboost_child_model <- function(df,
                                    indicator = indicator,
                                    indicator_family = "binomial",
                                    outputdir,
                                    region,
                                    covariates = all_fixed_effects,
                                    weight_column = "weight",
                                    xg_model_tune = TRUE,
                                    hyperparameter_filepath = NULL) {
  # load_R_packages(c("xgboost", "caret"))

  # Create stacking directory to save results
  stack_dir <- paste0(outputdir, "stackers/")
  dir.create(stack_dir, showWarnings = F)

  # Create model formula
  df <- as.data.table(df)
  setnames(df, indicator, "indicator")
  form <- as.formula(paste0("indicator ~ ", covariates))

  # Create custom weight column for xgboost, weight * sample size
  df[, xg_weight := get(weight_column) * N]

  # Make sure to model in prevalence space if binomial
  if (indicator_family == "binomial") {
    df[, indicator := indicator / N]
    objective_function <- "reg:logistic"
  }

  # If gaussian indicator make objective function linear
  if (indicator_family == "gaussian") objective_function <- "reg:linear"

  if (xg_model_tune == F & is.null(hyperparameter_filepath)) {
    stop("If you are not tuning xgboost you must provide a filepath to chosen hyperparameters./n
         Look at the hyperparameter_filepath argument to this function")
  }

  if (xg_model_tune == T) {
    message("Model tuning xgboost")

    # Set grid search as default unless filepath is provided
    if (is.null(hyperparameter_filepath)) {
      message("Tuning with default hyperparameter settings")
      xg_grid <- expand.grid(
        nrounds = 100,
        max_depth = c(4, 6, 8, 10, 12),
        eta = (3:8) / 100,
        colsample_bytree = .5,
        min_child_weight = 1,
        subsample = 1,
        gamma = 0
      )
    } else {
      message("Selecting pre-specified hyperparameter grid")
      hyperparam <- read.csv(hyperparameter_filepath)

      # Define grid search and
      xg_grid_final <- expand.grid(
        nrounds = hyperparam$nrounds,
        max_depth = hyperparam$max_depth,
        eta = hyperparam$eta,
        colsample_bytree = .5,
        min_child_weight = 1,
        subsample = 1,
        gamma = 0
      )
    }
    # Set cross validation options, default to 5 times repeated 5-fold cross validation
    # Selection function is "oneSE" to pick simplest model within one standard error of minimum
    train_control <- caret::trainControl(
      selectionFunction = "oneSE",
      method = "repeatedcv",
      number = 5,
      repeats = 5
    )
    # Fit model
    xg_fit <- caret::train(form,
      data = df,
      trControl = train_control,
      verbose = F,
      tuneGrid = xg_grid,
      metric = "RMSE",
      method = "xgbTree",
      objective = objective_function,
      weights = df$xg_weight
    )

    # Save model fit object for future use
    saveRDS(xg_fit, paste0(outputdir, "stackers/xg_fit_", region, ".RDS"))

    # Create plot showing which hyperparameters were selected
    cv_index <- length(xg_fit$control$index)
    xg_fit$results <-
      xg_fit$results %>%
      mutate(
        RMSE_low = RMSE - (RMSESD / sqrt(cv_index)),
        RMSE_high = RMSE + (RMSESD / sqrt(cv_index))
      )

    gg1 <-
      ggplot(
        xg_fit$results,
        aes(
          x = eta,
          y = RMSE,
          color = factor(max_depth),
          shape = factor(max_depth)
        )
      ) +
      geom_line() +
      geom_point() +
      xlim(range(xg_fit$results$eta)[1], range(xg_fit$results$eta)[2]) +
      labs(x = "Learning rate") +
      facet_wrap(~nrounds) +
      theme_bw() +
      scale_color_discrete("Max Tree Depth") +
      scale_shape_discrete("Max Tree Depth")

    error_width <- diff(range(xg_fit$results$eta)) / 50

    gg1 <- gg1 +
      geom_errorbar(
        data = xg_fit$results[oneSE(xg_fit$results, "RMSE", cv_index, F), ],
        aes(x = eta, ymin = RMSE_low, ymax = RMSE_high),
        alpha = 0.8,
        width = error_width,
        size = 0.5,
        color = "black"
      )

    ggsave(
      filename = paste0(stack_dir, region, "_hyperparameter.png"),
      plot = gg1
    )

    # Save the best parameters to csv file
    write.csv(xg_fit$bestTune, paste0(stack_dir, "xgboost_best_tune_", region, ".csv"))
  }

  if (xg_model_tune == T) {
    # Extract best parameters
    xg_best_tune <- read.csv(paste0(stack_dir, "xgboost_best_tune_", region, ".csv"))
  } else {
    # Extract best parameters from filepath
    xg_best_tune <- read.csv(hyperparameter_filepath)
  }

  # Define grid search and
  xg_grid_final <- expand.grid(
    nrounds = xg_best_tune$nrounds,
    max_depth = xg_best_tune$max_depth,
    eta = xg_best_tune$eta,
    colsample_bytree = .5,
    min_child_weight = 1,
    subsample = 1,
    gamma = 0
  )

  train_control_final <- caret::trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final"
  )

  message("Fitting xgboost on final tuned hyperparameters")
  xg_fit_final <- caret::train(form,
    data = df,
    trControl = train_control_final,
    verbose = F,
    tuneGrid = xg_grid_final,
    metric = "RMSE",
    method = "xgbTree",
    objective = objective_function,
    weights = df$xg_weight
  )

  # Plot the covariate importance of final model
  cov_plot <-
    ggplot(varImp(xg_fit_final, scale = FALSE)) +
    labs(x = "Covariate", y = "Relative Importance") +
    theme_bw()
  ggsave(
    filename = paste0(stack_dir, region, "_covariate_importance.png"),
    plot = cov_plot
  )

  # Extract out of sample and in sample predictions
  df[, "xgboost_cv_pred" := arrange(xg_fit_final$pred, rowIndex)[, "pred"]]
  df[, "xgboost_full_pred" := predict(xg_fit_final, df)]

  # Name model for later use in making stack rasters
  xg_fit_final$model_name <- "xgboost"

  xgboost <- list(
    dataset = df[, c("xgboost_cv_pred", "xgboost_full_pred")],
    xgboost = xg_fit_final
  )
  return(xgboost)
}
