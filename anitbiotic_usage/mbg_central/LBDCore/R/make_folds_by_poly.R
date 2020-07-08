#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cts_in_polys PARAM_DESCRIPTION
#' @param pt_poly_map PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_folds_by_poly
#' @export
make_folds_by_poly <- function(cts_in_polys,
                               pt_poly_map,
                               n_folds,
                               ...) {
  ## cts_in_polys:matrix containing two cols. 1st col is shape_ident
  ##    which identifies polygons in shapefile. 2nd col is count in shape
  ## pt_poly_map: vector as long as dataframe. each entry contains
  ##    the map from data to polygon shape_ident

  ## this function randomizes admin unit order and then sequentially
  ## adds to each fold until the sample size in the fold reaches
  ## 1/n_folds of the total count

  ## to make sure that the last one isn't much smaller, half of the
  ## folds take out the last poly that pushed them over the
  ## allowable count

  ## randomize the order

  if (n_folds > sum(as.numeric(cts_in_polys[, 2]) > 0)) {
    message("You have too little data somewhere to split into ", n_folds, " folds")
    message("Check the sample size in each strata, and each strata after time holdouts")
    message("Still, we will assign the available data randomly to folds...")

    fold_vec <- sample(x = 1:n_folds, size = sum(as.numeric(cts_in_polys[, 2]) > 0), replace = F)

    message("The sample size sum in each fold is: \n")
    pp.ind <- 1
    for (i in 1:n_folds) {
      if (i %in% fold_vec) {
        message(sprintf("Fold %i: %s", i, cts_in_polys[which(cts_in_polys[, 1] == pt_poly_map[pp.ind]), 2]))
        pp.ind <- pp.ind + 1
      } else {
        message("0")
      }
    }

    return(fold_vec)
  }

  rand_ord <- sample(1:nrow(cts_in_polys))

  ## randomly decide if the first one will include the final poly
  flip <- sample(0:1, 1)
  if (flip == 0) {
    include_final <- rep(0:1, ceiling(n_folds / 2))
  } else {
    include_final <- rep(1:0, ceiling(n_folds / 2))
  }

  ## get the sample size threshhold in each poly
  total_ct <- sum(as.numeric(cts_in_polys[, 2]))
  max_fold_ct <- ceiling(total_ct / n_folds)

  ## add polys to folds
  fold_sums <- rep(NA, n_folds)
  start.ind <- 1
  stop.ind <- 1
  for (fold in 1:(n_folds - 1)) {

    ## check threshhold
    while (sum(as.numeric(cts_in_polys[rand_ord[start.ind:stop.ind], 2])) < max_fold_ct &
      stop.ind < nrow(cts_in_polys)) {
      stop.ind <- stop.ind + 1
    }

    ## check if final poly is included
    if (include_final[fold] == 1) {
      stop.ind <- stop.ind - 1
    }

    ## store all the polys in the fold
    assign(
      paste0("polys_in_fold_", fold),
      cts_in_polys[rand_ord[start.ind:stop.ind], 1]
    )

    ## record total in fold
    total_ct[fold] <- sum(as.numeric(cts_in_polys[rand_ord[start.ind:stop.ind], 2]))

    ## adjust indices
    start.ind <- stop.ind + 1
    stop.ind <- start.ind + 1
  }

  ## and the last fold is everything else
  stop.ind <- length(rand_ord)
  fold <- n_folds
  assign(
    paste0("polys_in_fold_", fold),
    cts_in_polys[rand_ord[start.ind:stop.ind], 1]
  )
  total_ct[fold] <- sum(as.numeric(cts_in_polys[rand_ord[start.ind:stop.ind], 2]))

  message("The sample size sum in each fold is: \n")
  for (i in 1:n_folds) {
    message(sprintf("Fold %i: %s", i, total_ct[i]))
  }

  ## now we have the polys in different folds. we make a
  ## vector for which data rows are in the folds
  fold_vec <- rep(NA, length(pt_poly_map))
  for (fold in 1:n_folds) {
    fold_rows <- which(pt_poly_map %in% get((paste0("polys_in_fold_", fold))))
    fold_vec[fold_rows] <- fold
  }

  ## lastly, some of the points may not have fallen in the shapefiles
  ## for the moment they get randomly assigned to folds
  ## TODO: map these to nearest polys somehow
  if (sum(is.na(pt_poly_map)) > 0) {
    last_folds <- cut(seq(1, sum(is.na(pt_poly_map))),
      breaks = n_folds, labels = 1:n_folds
    )
    last_folds <- sample(as.numeric(as.character(last_folds)))
    fold_vec[which(is.na(pt_poly_map))] <- last_folds
  }

  ## return a vector placing each data row in a fold
  return(fold_vec)
}
