#' @title Save outputs from fractional counts raking
#'
#' @description Save outputs from fractional count raking
#' coming out of \code{fractionally_rake_counts} call.
#'
#' @param output_list The list of outputs from \code{fractionally_rake_counts} call.
#' @param sharedir Output to directory
#' @param indicator Indicator
#' @param age Age
#' @param reg Region
#'
#' @param holdout Holdout
#'
#' @examples
#' \dontrun{
#' raked_frax_counts_save(
#'   output_list = outputs,
#'   sharedir = "/share/geospatial/mbg/training/tr_had_diarrhea",
#'   indicator = "tr_had_diarrhea",
#'   age = 0,
#'   reg = "tza",
#'   holdout = 0
#' )
#' }
#' 
#' @rdname raked_frax_counts_save
#' @export
raked_frax_counts_save <- function(output_list, sharedir, indicator, age, reg, holdout) {

  ## Save raking factors
  fwrite(output_list[["raking_factors"]],
    file = paste0(sharedir, "/output/", run_date, "/", indicator, "_", reg, "_rf.csv")
  )

  # Save raked rates cell_pred object (which is equal to counts values!!)

  #### Need to unpack it, otherwise R gets sad ####
  raked_cell_pred <- output_list[["raked_cell_pred"]]

  save(raked_cell_pred,
    file = paste0(sharedir, "/output/", run_date, "/", indicator, "_raked_cell_draws_eb_bin", age, "_", reg, "_", holdout, ".RData")
  )

  ## Save raked counts aggregations
  raked_adm0_draws <- output_list[["raked_adm0_draws"]]
  raked_adm1_draws <- output_list[["raked_adm1_draws"]]
  raked_adm2_draws <- output_list[["raked_adm2_draws"]]
  save(raked_adm0_draws, raked_adm1_draws, raked_adm2_draws,
    file = paste0(main_dir, indicator, "_", "raked", "_admin_draws_eb_bin", age, "_", reg, "_", holdout, ".RData")
  )

  ## save unraked counts aggregations
  unraked_adm0_draws <- output_list[["unraked_adm0_draws"]]
  unraked_adm1_draws <- output_list[["unraked_adm1_draws"]]
  unraked_adm2_draws <- output_list[["unraked_adm2_draws"]]

  save(unraked_adm0_draws, unraked_adm1_draws, unraked_adm2_draws,
    file = paste0(main_dir, indicator, "_", "unraked", "_admin_draws_eb_bin", age, "_", reg, "_", holdout, ".RData")
  )


  return(0)
}
