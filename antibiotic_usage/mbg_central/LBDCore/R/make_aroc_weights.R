#' @title Make AROC weights
#' @description Generates weights to use in the weighted mean calculation of rate of changes
#' between adjacent model output years.
#'
#' @param weighting_res Spatial resolution to apply weights. Can be done by either:
#'    'domain' (e.g. Africa),
#'    'country'.
#'    In the future this could be expanded to adm* levels. Currently this only affects
#'    weighting if you also select weighting_type==empirical
#' @param weighting_type Method to generate the weighting used in the weighted-mean of the
#'    rate of changes calculated between all adjacent pairs of years in year_list.
#'    Can be: 'exponential' (see pow argument for details) or 'empirical'.
#'    Empirical generates weights proportional to the amount of data in each year.
#'      - The total sample size across all years divided by num_years is added to every year
#'        to ensure no pairs of years get zero weight
#'      - If empirical and country are both selected and there are countries with no data then
#'        exponential weights are used
#' @param year_list Vector (integer) of years included in the model run / cell pred object
#' @param pow Power used in year-weighting scheme:
#'    exponential weight used in determining year_wt_i:
#'         (yr_i - yr_1)/(yr_n - yr_i))^pow
#'    if pow==0, get uniform wts
#'    if pow==1, get linear wts
#'    if pow > 1, get exponential wts, etc.
#' @param input_data Required to derive emprical weights. A set of cleaned and collapsed micro
#'    data (e.g. input to MBG models) that will be used to derive the emprical weights.
#'    Required Columns:
#'       - country (iso3)
#'       - N
#'       - year
#' @param mult_emp_exp if TRUE and you are calculating emprical weights, the sample-size
#'    driven empirical weights are also multipled by the exponential weighting scheme such
#'    that both the exponential weight and the empirical weight contribute to the final weights
#' @return writes cell- and admin- level aroc objects to standard directories and formats
#'   in the 'pred_derivatives' folder of the model run.  cell-level objects are in the
#'   cell_pred indexed format. Both cell- and admin- aroc objects are matrices wide by draw.
#' @examples
#' \dontrun{
#' make_aroc_weights(
#'   weighting_res = "country",
#'   weighting_type = "empirical",
#'   pow = 1,
#'   input_data = data,
#'   mult_emp_exp = TRUE
#' )
#' }
#' @export
make_aroc_weights <- function(weighting_res,
                              weighting_type,
                              year_list,
                              pow = 1,
                              input_data,
                              mult_emp_exp) {
  num_yrs <- length(year_list)

  ## if exponential, then there's no difference by weighting_res so we just return a vector
  exp_wt <- 1:(num_yrs - 1)^pow
  exp_wt <- exp_wt / sum(exp_wt)
  if (weighting_type == "exponential") {
    return(exp_wt)
  }

  if (weighting_type == "empirical") { ## get weights based on input_data
    if (weighting_res == "domain") { ## return a single vector to use for all locations
      ## the weights are proportional to the number of data in pairs of adjacent years.
      ## to avoid 0 weights we add the total sample size/num_yrs to each year first
      emp_ct <- aggregate(N ~ year, input_data, sum)
      if (dim(emp_ct)[1] == num_yrs) {
        emp_ct_all_yr <- emp_ct$N
      } else {
        emp_ct_all_yr <- rep(0, num_yrs)
        for (ii in 1:length(year_list)) {
          if (year_list[ii] %in% emp_ct$year) {
            emp_ct_all_yr[ii] <- emp_ct$N[which(emp_ct$year == year_list[ii])]
          } else {
            emp_ct_all_yr[ii] <- 0
          }
        } ## ii over year_list
      }
      emp_ct <- emp_ct_all_yr + mean(emp_ct_all_yr)
      emp_wt <- (emp_ct + c(emp_ct[-1], 0))[-length(emp_ct)] / 2 ## average of adjacent counts
      if (mult_emp_exp == TRUE) emp_wt <- emp_wt * exp_wt
      emp_wt <- emp_wt / sum(emp_wt)
      return(emp_wt)
    }

    if (weighting_res == "country") {
      ## return a matrix of weights with the first column is the iso3 country code
      ## each row consists of num_yrs-1 weights to apply to that country.
      ## otherwise, we're doing the same thing as in

      all_ctry <- as.character(sort(unique(input_data$country)))
      emp_wts <- cbind(all_ctry, as.data.frame(matrix(0.00,
        ncol = num_yrs - 1,
        nrow = length(all_ctry)
      )))
      colnames(emp_wts)[1] <- "country"
      for (cc in 1:length(all_ctry)) {
        ctry <- all_ctry[cc]
        ctry_dat <- subset(input_data, country == ctry)

        ## same as in empirical & domain
        emp_ct <- aggregate(N ~ year, ctry_dat, sum)
        if (dim(emp_ct)[1] == num_yrs) {
          emp_ct_all_yr <- emp_ct$N
        } else {
          emp_ct_all_yr <- rep(0, num_yrs)
          for (ii in 1:length(year_list)) {
            if (year_list[ii] %in% emp_ct$year) {
              emp_ct_all_yr[ii] <- emp_ct$N[which(emp_ct$year == year_list[ii])]
            } else {
              emp_ct_all_yr[ii] <- 0
            }
          } ## ii over year_list
        }
        emp_ct <- emp_ct_all_yr + mean(emp_ct_all_yr)
        emp_wt <- (emp_ct + c(emp_ct[-1], 0))[-length(emp_ct)] / 2 ## average of adjacent counts
        if (mult_emp_exp == TRUE) emp_wt <- emp_wt * exp_wt
        emp_wt <- emp_wt / sum(emp_wt)

        ## store in country matrix
        emp_wts[cc, 2:ncol(emp_wts)] <- emp_wt
      }

      return(emp_wts)
    }
  }

  message("For some reason this function, make_aroc_weights(), failed to return anything")
}
