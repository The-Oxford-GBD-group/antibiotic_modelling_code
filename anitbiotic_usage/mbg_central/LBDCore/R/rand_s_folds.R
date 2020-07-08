#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param n_folds PARAM_DESCRIPTION, Default: 5
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname rand_s_folds
#' @export
rand_s_folds <- function(xy, ## xy location matrix
                         ss = 1, ## sample size vec (or 1 if equi-ss)
                         n_folds = 5,
                         ...) {


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (4) random folds in space
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  if (length(ss) < n_folds) {
    message("Too many folds for too few countries! Expand your horizons")
    stop()
  }

  if (length(ss) != nrow(xy)) {
    message("length of ss and nrow(xy) must match!")
    stop()
  }

  if (length(ss) == 1) ss <- rep(1, nrow(xy))

  total.ct <- sum(ss)
  max.fold.ct <- ceiling(total.ct / n_folds)

  ## make a vector to store folds
  fold.vec <- numeric(nrow(xy))

  ## randomize the order of the rows
  rand.ord <- sample(1:nrow(xy))

  ## randomly decide if the first one will include the final poly
  flip <- sample(0:1, 1)
  if (flip == 0) {
    include.final <- rep(0:1, ceiling(n_folds / 2))
  } else {
    include.final <- rep(1:0, ceiling(n_folds / 2))
  }

  fold.sums <- rep(NA, n_folds)
  start.ind <- 1
  stop.ind <- 1
  for (fold in 1:(n_folds - 1)) {

    ## check threshhold
    while (sum(ss[rand.ord[start.ind:stop.ind]]) < max.fold.ct) {
      stop.ind <- stop.ind + 1
    }

    ## check if final row is included
    if (include.final[fold] == 1) {
      stop.ind <- stop.ind - 1
    }

    ## identify the selected rows as being in this fold
    fold.vec[rand.ord[start.ind:stop.ind]] <- fold

    ## record total in fold
    total.ct[fold] <- sum(ss[rand.ord[start.ind:stop.ind]])

    ## adjust indices
    start.ind <- stop.ind + 1
    stop.ind <- start.ind + 1
  }

  ## the last fold is everything else
  stop.ind <- nrow(xy)
  fold.vec[rand.ord[start.ind:stop.ind]] <- n_folds
  total.ct[n_folds] <- sum(ss[rand.ord[start.ind:stop.ind]])

  ## print ss sums in folds
  message("The sum in each different fold is: \n")
  for (i in 1:n_folds) {
    message(total.ct[i])
  }

  return(fold.vec)
}
