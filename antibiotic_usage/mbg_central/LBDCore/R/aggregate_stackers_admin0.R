#' @title Create national estimates from stackes
#' @description Create national estimates from stackers by population-weighting the pixel estimates.
#'
#' @param indicator indicator
#' @param indicator_group indicator_group
#' @param run_date model run date
#' @param age age group
#' @param holdout model holdout number
#' @param regions vector of model regions
#' @param year_list vector of modeled years
#' @param pop_measure population measure to use for population-weighting
#' @param stackers_logit_transform logical. Were stackers logit-transformed when fitting the final model?
#' @param predictor_logit_transform logical. Was the linear predictor logit-transformed (eg, as is
#'        typically the case in binomial models)?
#' @param results_file file path to a .csv file where results should be saved. Optional.
#' @param shapefile_version string specifies which shapefile version to pull
#'
#' @return A table of national-level estimates for each country, year, and stacker. If results_file
#'         is provided, this information is saved to the specified file. If results_file is not
#'         provided, this information is returned as a data.table.
#'
#' @export
aggregate_stackers_admin0 <- function(indicator, indicator_group, run_date, age, holdout,
                                      regions, year_list, pop_measure, stackers_logit_transform,
                                      predictor_logit_transform, results_file = NULL,
                                      shapefile_version = "current") {

  # Loop over regions
  all_regions <- lapply(regions, function(reg) {
    message(paste("Working on", reg, "\n"))
    pathaddin <- paste0("_bin", age, "_", reg, "_", holdout)

    # Get simple polygon and simple raster
    poly <- load_simple_polygon(
      gaul_list = get_adm0_codes(reg, shapefile_version = shapefile_version),
      buffer = 0.4, subset_only = FALSE,
      shapefile_version = shapefile_version
    )
    subset_shape <- poly[[1]]
    simple_polygon <- poly[[2]]
    simple_raster <- build_simple_raster_pop(subset_shape)[["simple_raster"]]
    pixel_id <- notMissingIdx(simple_raster)

    # Get population
    pop <- load_and_crop_covariates_annual(
      covs = "worldpop", measures = pop_measure, simple_polygon = simple_polygon,
      start_year = min(year_list), end_year = max(year_list), interval_mo = 12, agebin = 1
    )
    pop <- crop_set_mask(pop[[1]], simple_raster)
    pop <- data.table(raster::extract(pop, pixel_id))
    pop[, pixel_id := pixel_id]
    pop <- melt(pop, id.vars = "pixel_id", variable.name = "year", value.name = "pop")
    pop[, year := min(year_list) + as.numeric(year) - 1]
    pop[is.na(pop), pop := 0]

    # Get stackers
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/", run_date, pathaddin, ".RData"))
    cov_list <- cov_list[child_model_names]
    cov_list <- lapply(cov_list, function(x) raster::extract(x, pixel_id))
    cov_list <- data.table(do.call("cbind", cov_list))
    cov_list[, pixel_id := pixel_id]
    cov_list <- melt(cov_list,
      id.vars = "pixel_id", measure = patterns(child_model_names),
      variable.name = "year", value.name = child_model_names
    )
    cov_list[, year := min(year_list) + as.numeric(year) - 1]

    # Get combined stackers result based on INLA coefficients
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/", indicator, "_model_eb", pathaddin, ".RData"))
    if ("sdrep" %in% names(res_fit)) { # TMB, w/ stackers as fixed effects
      coef <- res_fit$sdrep$par.fixed[names(res_fit$sdrep$par.fixed) == "alpha_j"]
      names(coef) <- res_fit$fenames
      int <- coef["int"]
      coef <- coef[child_model_names]
    } else if ("covar" %in% names(res_fit$summary.random)) { # INLA, w/ stackers as random effects (to sum to one)
      coef <- res_fit$summary.random$covar$"mean"
      names(coef) <- child_model_names
      int <- res_fit$summary.fixed["int", "mean"]
    } else { # INLA, w/ stackers as fixed effects
      coef <- res_fit$summary.fixed[child_model_names, "mean"]
      names(coef) <- child_model_names
      int <- res_fit$summary.fixed["int", "mean"]
    }

    if (stackers_logit_transform) {
      cov_list[, combined := int + Reduce("+", lapply(child_model_names, function(x) logit(cov_list[[x]]) * coef[x]))]
    } else {
      cov_list[, combined := int + Reduce("+", lapply(child_model_names, function(x) cov_list[[x]] * coef[x]))]
    }

    if (predictor_logit_transform) cov_list[, combined := inv.logit(combined)]

    # Merge simple_raster, stackers, and population
    admin0 <- data.table(pixel_id = pixel_id, ADM0_CODE = raster::extract(simple_raster, pixel_id))
    admin0 <- merge(admin0, cov_list, by = "pixel_id")
    admin0 <- merge(admin0, pop, by = c("pixel_id", "year"))
    if (nrow(admin0) != nrow(pop) | nrow(admin0) != nrow(cov_list)) stop("dimension mismatch")

    # Calculate population-weighted national averages
    admin0[, lapply(.SD, function(x) weighted.mean(x, pop, na.rm = T)), by = "ADM0_CODE,year", .SDcols = c(child_model_names, "combined")]
  })

  # Save or return results for all regions
  all_regions <- rbindlist(all_regions)
  if (!is.null(results_file)) {
    write.csv(all_regions, file = results_file, row.names = F)
    return(paste("results saved to:", results_file))
  } else {
    return(all_regions)
  }
}
