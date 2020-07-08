#' @title Raking predictions
#' @description Rakes estimates to gold standard estimates (usually GBD)
#' @param raking_factors output from calc_raking_factors
#' @param pop_wts_object output from make_population_weights
#' @param cell_pred Cells by Draws matrix which is output from predict_mbg()
#' @param logit_rake do logit raking?, Default: FALSE
#' @return eturns cell_preds raked
#' @rdname rake_predictions
#' @export
rake_predictions <- function(raking_factors,
                             pop_wts_object,
                             cell_pred,
                             logit_rake = FALSE) {

  ## Replicate adm codes by years
  adm_codes_all <- pop_wts_object$adm_code
  adm_codes_all[adm_codes_all == "NA"] <- NA
  periods <- rep(unique(raking_factors$year), each = length(adm_codes_all))
  adm_codes_all <- paste(adm_codes_all, periods, sep = "_")

  # merge adm codes and raking factor
  z <- data.frame(ID = adm_codes_all, order = 1:length(adm_codes_all))
  z$ID <- as.character(z$ID)
  raking_factors$ID <- paste(raking_factors$name, raking_factors$year, sep = "_")
  factors_all <- merge(z, raking_factors[, c("ID", "raking_factor"), with = FALSE], by = "ID", all.x = T)
  factors_all <- factors_all[order(factors_all$order), ]

  # make a results frame
  raked <- matrix(NA, nrow = nrow(cell_pred), ncol = ncol(cell_pred))

  if (dim(factors_all)[1] != dim(raked)[1]) {
    stop("Factors_all and Raked dimensions do not match")
  }

  # multiply through the raking factor
  message(sprintf("raking %i draws", dim(raked)[2]))
  pb <- txtProgressBar(min = 0, max = dim(raked)[2], initial = 0)
  if (logit_rake == FALSE) {
    for (i in 1:dim(raked)[2]) {
      setTxtProgressBar(pb, i)
      raked[, i] <- cell_pred[, i] * factors_all$raking_factor
    }
  }
  if (logit_rake == TRUE) {
    for (i in 1:dim(raked)[2]) {
      setTxtProgressBar(pb, i)
      raked[, i] <- ilogit(logit(cell_pred[, i]) + factors_all$raking_factor)
    }
  }
  close(pb)

  return(raked)
}
