## get_pv_admin function ###################################################

#' @title get_pv_admin
#' @description This function collapses input data to admin 0/admin 1/admin 2 using the `input_aggregate_admin` function
#' See `input_aggregate_admin` function documentation for input data requirements
#'
#' To use this function, your input dataset must have a column that is the sum of the sample weights
#'
#' This function pulls the input data from the model directory and aggregated admin summaries created in the `aggregate_results.R` script
#'
#' This function returns average RMSE, Bias, Mean Absolute Error, and Standard Error for admin 0 /admin 1 / admin 2
#'
#' @author Lauren Woyczynski, \email{lpw3@uw.edu}
#'
#' @param indicator indicator name used in file structure for mbg
#' @param indicator_group indicator group
#' @param strata Regions specified from your model
#' @param run_date  model run date
#' @param input_data If specified, provides the preloaded input data so the function does not look in your model directory
#' @param indicator_family If specified as Gaussian, this makes sure to not divide the prevalence by N which is required for binomial indicators
#' @param nic_col This is the name of the unique survey variable, usually labeled "nid" but other teams might use different terminology
#' @param samp_col This is the name of the column that contains the sum of the sample weights for the collapsed points.
#' @param shapefile_version String indicating version of shapefile to pull
#' @param input_file If your team does not use the standard `/share/geospatial/mbg/input_data/{indicator}.csv` input data filepath, supply the filepath to your data here
#' @param admin0_file If your team does not use the standard `'/share/geospatial/mbg/',indicator_group, '/', indicator, '/output/',run_date, '/pred_derivatives/admin_summaries/',indicator, '_admin_0_unraked_summary.csv'` directory to save unraked aggregated admin summaries, supply the correct filepath here
#' @param admin1_file If your team does not use the standard `'/share/geospatial/mbg/',indicator_group, '/', indicator, '/output/',run_date, '/pred_derivatives/admin_summaries/',indicator, '_admin_1_unraked_summary.csv'` directory to save unraked aggregated admin summaries, supply the correct filepath here
#' @param admin2_file If your team does not use the standard `'/share/geospatial/mbg/',indicator_group, '/', indicator, '/output/',run_date, '/pred_derivatives/admin_summaries/',indicator, '_admin_2_unraked_summary.csv'` directory to save unraked aggregated admin summaries, supply the correct filepath here
#'
#' @return a table with bias, rmse, mae, and se for each admin level
#' @export


get_admin_pv <- function(indicator,
                         indicator_group,
                         run_date,
                         input_file = NULL,
                         shapefile_version = "current",
                         samp_col = "sum_of_sample_weights",
                         strata = strata,
                         indicator_family = "binomial",
                         nid_col = "nid",
                         admin0_file = NULL,
                         admin1_file = NULL,
                         admin2_file = NULL) {
  message("Pulling in input data...")
  if (is.null(input_file)) {
    if (!file.exists(paste0("/share/geospatial/mbg/input_data/", indicator, ".csv"))) stop("Indicator input data does not exist in /share - please specify a file path with the input_file arg")
    input.dt <- fread(paste0("/share/geospatial/mbg/input_data/", indicator, ".csv"))
  } else {
    input.dt <- fread(input_file)
  }

  if (!("point" %in% colnames(input.dt))) stop("Your input dataset needs a binary var called point")
  if (!(samp_col %in% colnames(input.dt))) stop("Your input dataset needs a column that is the sum of sample weights")

  message("Collapsing data to admin levels...")
  admin_data <- input_aggregate_admin(
    indicator = indicator, indicator_group, run_date = run_date, sample_column = samp_col,
    input_data = input.dt, regions = strata, indicator_family = ind_fam, svy_id = nid_col, shapefile_version = shapefile_version
  )

  ad0_data <- admin_data$ad0
  ad1_data <- admin_data$ad1
  ad2_data <- admin_data$ad2

  # Read in admin level results
  message("Pulling in admin results")
  if (is.null(admin0_file)) {
    ad0_mbg <- fread(paste0(
      "/share/geospatial/mbg/",
      indicator_group, "/",
      indicator, "/output/",
      run_date, "/pred_derivatives/admin_summaries/",
      indicator, "_admin_0_unraked_summary.csv"
    ))
  } else {
    ad0_mbg <- fread(admin0_file)
  }
  if (is.null(admin1_file)) {
    ad1_mbg <- fread(paste0(
      "/share/geospatial/mbg/",
      indicator_group, "/",
      indicator, "/output/",
      run_date, "/pred_derivatives/admin_summaries/",
      indicator, "_admin_1_unraked_summary.csv"
    ))
  } else {
    ad1_mbg <- fread(admin1_file)
  }
  if (is.null(admin2_file)) {
    ad2_mbg <- fread(paste0(
      "/share/geospatial/mbg/",
      indicator_group, "/",
      indicator, "/output/",
      run_date, "/pred_derivatives/admin_summaries/",
      indicator, "_admin_2_unraked_summary.csv"
    ))
  } else {
    ad2_mbg <- fread(admin2_file)
  }

  # Merge nid and mbg results

  ad0 <- merge(ad0_data, ad0_mbg, by = c("ADM0_NAME", "ADM0_CODE", "year"))
  ad1 <- merge(ad1_data, ad1_mbg, by = c("ADM0_NAME", "ADM0_CODE", "ADM1_NAME", "ADM1_CODE", "year"))
  ad2 <- merge(ad2_data, ad2_mbg, by = c("ADM0_NAME", "ADM0_CODE", "ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "year"))


  # Biasa: paNID-mean(pa,yNID,1mbg, ... ,pa,yNID,ndrawsmbg)
  # MAEa: |paNID-mean(pa,yNID,1mbg, ... ,pa,yNID,ndrawsmbg)|
  # SEa: (paNID-mean(pa,yNID,1mbg, ... ,pa,yNID,ndrawsmbg))2
  # and then we average over admin(-years) and take the square root to get RMSE across admin(-years)
  message("Calculate predictive validity...")

  ad0[, bias := outcome - mean]
  ad0[, mae := abs(bias)]
  ad0[, se := bias^2]

  ad1[, bias := outcome - mean]
  ad1[, mae := abs(bias)]
  ad1[, se := bias^2]

  ad2[, bias := outcome - mean]
  ad2[, mae := abs(bias)]
  ad2[, se := bias^2]

  pv_table <- data.table(
    ad_level = c(0, 1, 2),
    rmse = c(sqrt(mean(ad0$se, na.rm = T)), sqrt(mean(ad1$se, na.rm = T)), sqrt(mean(ad2$se, na.rm = T))),
    bias = c(weighted.mean(ad0$bias, ad0$N, na.rm = T), weighted.mean(ad1$bias, ad1$N, na.rm = T), weighted.mean(ad2$bias, ad2$N, na.rm = T)),
    mae = c(weighted.mean(ad0$mae, ad0$N, na.rm = T), weighted.mean(ad1$mae, ad1$N, na.rm = T), weighted.mean(ad2$mae, ad2$N, na.rm = T)),
    se = c(weighted.mean(ad0$se, ad0$N, na.rm = T), weighted.mean(ad1$se, ad1$N, na.rm = T), weighted.mean(ad2$se, ad2$N, na.rm = T))
  )

  return(pv_table)
}
