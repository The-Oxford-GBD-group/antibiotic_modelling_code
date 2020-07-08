#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pop_wts_object PARAM_DESCRIPTION
#' @param cell_pred PARAM_DESCRIPTION
#' @param admin_level PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION, Default: c(2000, 2005, 2010, 2015)
#' @param gaul_list PARAM_DESCRIPTION, Default: gaul_list
#' @param fun PARAM_DESCRIPTION, Default: 'mean'
#' @param summarize PARAM_DESCRIPTION, Default: FALSE
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_condSim
#' @export
make_condSim <- function(pop_wts_object,
                         cell_pred,
                         admin_level,
                         years = c(2000, 2005, 2010, 2015),
                         gaul_list = gaul_list,
                         fun = "mean",
                         summarize = FALSE,
                         shapefile_version = "current") {

  #################################################################################
  ### Wrapper for condSim from seegMBG
  ## Inputs:
  # pop_wts_object: list object that is output of make_population_weights()
  # cell_pred: Cells by Draws matrix which is output from predict_mbg()
  # years: vector of years in your analysis (should match population as well)
  # fun: Mean by default. Other functions include gini
  # summarize: Logical, if TRUE return only summary and not full draws
  # shapefile_version: string indicating shapefile version to pull
  ## Outputs: returns a matrixs of (number of admin units * number of years in analysis)
  ##          rows by #draws in cell_pred columns
  #################################################################################
  # ease
  p <- pop_wts_object

  message(sprintf("Aggregating results by draw to the admin %i level. If you want a different aggregation level, re-run make_population_weights with a different value for the admin_level argument. \n\n", p$admin_level))

  # make pop_wt long
  pop_wt <- as.vector(p$pop_wt)

  # throw exception if data are non-conformable
  if (length(pop_wt) != dim(cell_pred)[1]) {
    stop("Population Weights not same number of cells as cell predictions.")
  }


  # get admin gauls (or names) at each cell. Repeat for number of years
  yrs <- length(pop_wt) / length(p$adm_code)
  message(sprintf("There are %i years of data here. Modeler please be sure your population years and data years are aligned. Current input is %s \n\n", yrs, paste(years, collapse = " ")))

  p$adm_code[p$adm_code == "NA"] <- NA
  periods <- rep(years, each = length(p$adm_code))
  adm_code_all <- paste(p$adm_code, periods, sep = "_")

  # get NA mask
  good_cells <- which(!is.na(rep(p$adm_code, yrs)))
  good_cells <- good_cells[which(!is.na(cell_pred[good_cells, 1]))]
  good_cells <- good_cells[which(!is.na(pop_wt[good_cells]))]

  # rescale pop weights if some got dropped
  dt <- data.table(pw = pop_wt[good_cells], ad = adm_code_all[good_cells])
  dt$pw[dt$pw == 0] <- 0.00000001 # so no areas have zero pop which leads to buggy behavior in 2 or 3 districts
  dt[, tot := sum(pw), by = ad]
  dt[, pw := pw / tot]
  pw <- dt$pw
  ad <- dt$ad

  # actual condSim run
  if (fun == "mean") {
    cond_sim_adm <- condSim(
      vals = cell_pred[good_cells, ],
      weights = pw, # pop_wt[good_cells],
      group = ad
    ) # adm_code_all[good_cells])
  } else {
    cond_sim_adm <- condSim(
      vals = cell_pred[good_cells, ],
      weights = pw, # pop_wt[good_cells],
      group = ad, # adm_code_all[good_cells],
      fun = fun
    )
  }

  # summarize if requested
  if (summarize == TRUE) {
    message(sprintf("Returning Summary (mean) made from %i draws. \n\n", dim(cell_pred)[2]))
    cond_sim_adm <- (apply(cond_sim_adm, 1, mean))
  } else {
    message(sprintf("Returning %i draws. \n\n", dim(cell_pred)[2]))
  }

  # Make sure we only return results for admin units in the list of adm0 gaul codes requested. There's the possibility that a few cells near the borders get counted for the adjacent countries.
  # Get list of admin* codes within selected admin0 codes
  adm_gaul_list <- get_adm_codes_subnat(gaul_list, admin_level,
    shapefile_version = shapefile_version
  )
  # Convert named vector to dataframe
  if (summarize) {
    tmp <- cbind(read.table(text = names(cond_sim_adm)), val = cond_sim_adm)
    # Make new column of just gaul codes
    tmp$gaul_code <- gsub("_.*", "", tmp$V1)
    # Subset
    tmp <- tmp[tmp$gaul_code %in% adm_gaul_list, ]
    # Convert back to dataframe
    clean_cond_sim_adm <- tmp$val
    names(clean_cond_sim_adm) <- tmp$V1

    return(clean_cond_sim_adm)
  } else {
    return(cond_sim_adm)
  }
}
