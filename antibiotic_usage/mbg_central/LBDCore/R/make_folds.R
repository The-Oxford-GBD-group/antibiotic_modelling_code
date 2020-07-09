#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param spat_strat PARAM_DESCRIPTION, Default: NULL
#' @param temp_strat PARAM_DESCRIPTION, Default: NULL
#' @param spte_strat PARAM_DESCRIPTION, Default: NULL
#' @param save.file PARAM_DESCRIPTION, Default: paste0("/share/geospatial/mbg/", indicator_group, "/", indicator,
#'    "/output/", run_date, "/stratum.rds")
#' @param ... PARAM_DESCRIPTION
#' @param strat_cols PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_folds
#' @export
make_folds <- function(data,
                       n_folds,
                       spat_strat = NULL,
                       temp_strat = NULL,
                       spte_strat = NULL,
                       save.file = paste0(
                         "/share/geospatial/mbg/",
                         indicator_group, "/",
                         indicator, "/output/",
                         run_date, "/stratum.rds"
                       ), ## place to save the final object from the function
                       ...,
                       #                   long_col,   ## needed for any spat_strat
                       #                   lat_col,    ## needed for any spat_strat
                       #                   ss_col,     ## needed for most strats
                       #                   yr_col,     ## needed for most temp_strats
                       #                   ct_col,     ## needed for ct spat_strat
                       strat_cols, ## needed for stratifying
                       seed) {


  ###################
  ## IN SPACE-TIME ##
  ###################
  ## first we obey the temporal choice, then split in space
  ## currently, these are all combos of space holdout funcs and time holdout funcs.


  ##############################
  ## OVERALL WRAPPER FUNCTION ##
  ##############################
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ##
  ## STEPS:
  ## 1st: subset data by strata if specified
  ## 2nd: fold by temporal strategy if chosen
  ## 3rd: fold by spatial  strategy if chosen
  ##
  ## INPUTS:
  ##
  ## data: full dataset to split into test/train
  ## n_folds: number of folds in test/train splits
  ## spat_strat: spatial  holdout strategy. one of: c('rand', 'poly', 'qt', 'ct')
  ## temp_strat: temporal holdout strategy. one of: c('rand', 'prop', 'prop_comb', 'yr', 'chrono')
  ## seed: RNG seed in case you'd like to be able to recreate folds
  ##
  ## OUTPUTS: 2 item list
  ## 1st item: 1 by nrow(data) vector containing integers identifying folds
  ## 2nd item: matrix of stratification combinations used to make holdouts
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


  ## setup for testing purposes
  ## n_folds = 5
  ## spat_strat = "qt"
  ## temp_strat = "prop"
  ## spte_strat = NULL
  ## strat_cols = "age_bin"
  ## yr_col = "yr"
  ## ss_col = "exposed"
  ## lat_col = "lat"
  ## long_col = "long"
  ## ts = 1000
  ## mb = 5


  ## ## make a dataset
  ## n = 10000
  ## data = data.frame(lat  = runif(n, 10, 30),
  ##                   long = runif(n, 0, 40),
  ##                   yr   = sample(rep(c(1995, 2000, 2005, 2010), n / 4), n),
  ##                   age_bin = sample(1:4, n, replace = T),
  ##                   exposed = sample(25:35, n, replace = T))

  ## simulate some data
  ## n <- 500
  ## yr <- c(2000, 2005, 2010, 2015)
  ## xy <- matrix(runif(n * length(yr) * 2), ncol = 2)
  ## ss <- sample(1:100, size = n * length(yr), replace = TRUE)
  ## age <- sample(1:4, size=n*length(yr), replace=TRUE)
  ## yrs <- sample(yr, size = n * length(yr), replace = TRUE)
  ## df <- as.data.frame(cbind(xy, ss, yrs, age))
  ## colnames(df) <- c("longitude", "latitude", "ss", "year", "age")
  ## ss_col   = "ss"  ;     yr_col = 'year'     ; spat_strat = 'rand'; temp_strat = 'prop'
  ## long_col = 'longitude'; lat_col = 'latitude'; n_folds = 5; strat_cols = 'age'
  ## folds = make_folds(data = df, n_folds = 5, spat_strat = 'rand', temp_strat = 'prop',
  ##                    long_col = 'longitude', lat_col = 'latitude', strat_cols = 'age')




  ## ~#####################
  ## Prepare for battle ##
  ## ~#####################

  ## for some reason, functions that work on data.frames don't all work on data.tables
  data <- as.data.frame(data)

  message("Identifying folds for validation")

  if (!is.null(spte_strat)) {
    message("B/C you've chosen a space-time strategy, any input into either spat_strat or temp_strat strategies will be ignored")
    spat_strat <- NULL
    temp_strat <- NULL
  }

  if (!is.null(temp_strat)) {
    if (temp_strat == "yr" | temp_strat == "chrono") {
      message("B/C you've chosen temp_strat==('yr'|'chrono'), I've set spat_strat to be NA")
      spat_strat <- NULL
    }
  }

  message(paste("You've selected:",
    paste0("spat_strat = ", spat_strat),
    paste0("temp_strat = ", temp_strat),
    paste0("spte_strat = ", spte_strat),
    sep = "\n"
  ))

  ## check for unused arguments and give warning if any won't be used
  params <- list(...)
  optional_params <- c(
    "ts", "mb", "plot_fn", "plot_shp", "shp_fn",
    "admin_raster", "shape_ident", "admin_shps",
    "ss_col", "mask_shape", "mask_raster",
    "long_col", "lat_col", "yr_col", "ts_vec"
  )
  unused_params <- setdiff(names(params), optional_params)
  if (length(unused_params)) {
    stop("You entered unused parameters! ", paste(unused_params, collapse = ", "))
  }

  ## set the seed if desired
  if (!(missing(seed))) set.seed(seed)


  ## ~##############################
  ## FIRST, split data by strata ##
  ## ~##############################
  print("Making stratum")

  if (!missing(strat_cols)) {

    ## get different stratum
    all_strat <- get_strat_combos(data = data, strat_cols = strat_cols)

    ## make a list. each element is a different strata of data
    stratum <- list(NULL)
    for (s in 1:nrow(all_strat)) {

      ## get all combos
      strata <- as.data.frame(all_strat[s, ])
      colnames(strata) <- colnames(all_strat)

      ## get rows in current strata
      strat_rows <- get_strat_rows(
        data = data,
        strata = strata
      )

      ## enter the data strata into the list
      stratum[[s]] <- data[strat_rows, ]
    }
  } else {
    stratum[[1]] <- data
  }

  m1 <- paste0("Length of data is: ", nrow(data))
  m2 <- paste0("Length of data in all strata is: ", sum(unlist(lapply(stratum, nrow))))
  print(m1)
  print(m2)
  print("These should be the same number. If not, maybe you have NAs to clean?")

  ## ~#############################
  ## SECOND, make folds in time ##
  ## ~#############################

  if (!is.null(temp_strat)) {
    print("Making time folds")

    ## we have a dictionary of temporal methods to call from
    t_dict <- list(
      "rand" = NULL,
      "prop" = proptime_folds,
      "prop_comb" = proptime_folds_combine,
      "yr" = yr_folds,
      "chrono" = chrono_folds
    )

    ## match the choice to the function and run it
    t_fun <- t_dict[[temp_strat]]
    if (!is.null(t_fun)) {
      ## TODO Parallelize
      t_stratum <- lapply(
        1:length(stratum),
        function(x) {
          yr <- stratum[[x]][[yr_col]]
          ss <- stratum[[x]][[ss_col]]
          ## get the fold indices
          t_folds <- t_fun(
            yr = yr,
            ss = ss,
            n_folds = n_folds,
            ...
          )
          ## make & return the subsetted datasets
          lapply(
            1:length(t_folds),
            function(y) {
              stratum[[x]][t_folds[[y]], ]
            }
          )
        }
      )
    } else {
      t_stratum <- lapply(stratum, function(x) list(x))
    }
  } else {
    t_stratum <- lapply(stratum, function(x) list(x))
  }

  ## identify time folds in each stratum and unlist back to stratum
  stratum <- NULL
  for (i in 1:length(t_stratum)) {
    for (j in 1:length(t_stratum[[i]])) {
      t_stratum[[i]][[j]] <- cbind(
        t_stratum[[i]][[j]],
        rep(j, nrow(t_stratum[[i]][[j]]))
      )
      colnames(t_stratum[[i]][[j]])[ncol(t_stratum[[i]][[j]])] <- "t_fold"
    }
    stratum[[i]] <- do.call(rbind, t_stratum[[i]])
  }

  m1 <- paste0("Length of data is: ", nrow(data))
  m2 <- paste0("Length of data in all strata is: ", sum(unlist(lapply(stratum, nrow))))
  print(m1)
  print(m2)
  print("These should be the same number. If not, maybe you have NAs to clean?")

  ## ~#############################
  ## THIRD, make folds in space ##
  ## ~#############################


  if (!is.null(spat_strat)) {
    print("Making space folds")

    ## we have a dictionary of spatial methods to call from
    s_dict <- list(
      "rand" = rand_s_folds,
      "poly" = ad2_folds,
      "qt" = quadtree_folds,
      "ct" = ct_folds
    )

    ## match the choice to the function and run it on the double
    ## list of data
    s_fun <- s_dict[[spat_strat]]
    if (!is.null(s_fun)) {
      ## TODO parallelize
      s_stratum <- lapply(
        1:length(stratum),
        function(x) {
          message(paste0("Making folds for stratum: ", x))
          ## first col is fold second col is holdout ID
          s_fold_hoid <- matrix(ncol = 2, nrow = nrow(stratum[[x]]))

          lapply(
            1:max(stratum[[x]][["t_fold"]]),
            function(y) {
              message(paste0("On temp fold: ", y))
              t_fold_r <- which(stratum[[x]][["t_fold"]] == y)
              xy <- cbind(
                stratum[[x]][
                  t_fold_r,
                  long_col
                ],
                stratum[[x]][
                  t_fold_r,
                  lat_col
                ]
              )
              colnames(xy) <- c("long", "lat")
              ss <- stratum[[x]][t_fold_r, ss_col]

              ## IF 'ts_vec' has been input, as opposed to just 'ts', we need to reassign that here
              extra.args <- list(...)
              if (!is.null(extra.args[["ts_vec"]])) {
                extra.args[["ts"]] <- extra.args[["ts_vec"]][x]
                extra.args[["ts_vec"]] <- NULL
              }
              all.args <- c(
                list(
                  "xy" = xy,
                  "ss" = ss,
                  "n_folds" = n_folds,
                  "stratum" = x,
                  "t_folds" = y
                ),
                extra.args
              )

              print("Get the fold indices")
              s_folds <- do.call(s_fun, all.args)

              s_fold_hoid[t_fold_r, ] <- s_folds
            }
          )
        }
      )

      ## unpack fold indices back to stratum
      for (i in 1:length(stratum)) {
        fold_hoid <- matrix(ncol = 2, nrow = nrow(stratum[[i]]))

        for (j in sort(unique(stratum[[i]][["t_fold"]]))) {
          t_fold_r <- which(stratum[[i]][["t_fold"]] == j)
          fold_hoid[t_fold_r, ] <- s_stratum[[i]][[j]]
          colnames(fold_hoid) <- c("fold", "ho_id")
        }
        stratum[[i]] <- cbind(stratum[[i]], fold_hoid)
      }
    }
  }

  m1 <- paste0("Length of data is: ", nrow(data))
  m2 <- paste0("Length of data in all strata is: ", sum(unlist(lapply(stratum, nrow))))
  print(m1)
  print(m2)

  ## ~######################################################
  ## FOURTH, if not the others, make folds in space-time ##
  ## ~######################################################

  if (!is.null(spte_strat)) {

    # get space-time folds
    if (spte_strat == "nids") {
      st_folds <- lapply(stratum, function(x) {
        nid_folds(nids = x$nid, yr = x[[yr_col]], ss = x[[ss_col]], n_folds)
      })
    } else {
      stop(paste(spte_strat, "is not a valid option for spte_strat"))
    }

    # copy fold IDs back to stratum
    for (i in 1:length(stratum)) {
      for (j in 1:length(st_folds[[i]])) {
        stratum[[i]][st_folds[[i]][[j]], "fold"] <- j
      }
    }
  }

  ## ~###########################################
  ## Lastly, if completely random is selected ##
  ## ~###########################################



  ## ############################
  ## FINISH w/ post-processing ##
  ## ############################

  ## recombine temporal stratification if needed (i.e. if )

  ## check it.
  ## table(data$fold,data$age_bin,dnn=list('fold','age_bin'))

  ## ## clean it up
  ## data=data[order(as.numeric(row.names(data))),]
  ## data$elig=NULL

  ## ## save to disk
  ## write.csv(data,
  ##           file = paste0('data/clean/mortality_combined.csv'),
  ##           row.names = FALSE)

  ## name the folds
  names(stratum) <- name_strata(all_strat = all_strat)

  ## save a copy of stratum
  saveRDS(file = save.file, object = stratum)

  ## return
  return(stratum)
}
