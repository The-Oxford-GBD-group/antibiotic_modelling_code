#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cr PARAM_DESCRIPTION, Default: core_repo
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname set_up_config
#'
#' @note Need to use an object instead of pushing to global scope
#'
#' @export
check_config <- function(cr = core_repo) {
  # must_haves <- read.csv(paste0(cr, "/mbg_central/share_scripts/common_inputs/config_must_haves.csv"), header = F, stringsAsFactors = F)$V1
  data(must_haves)

  message("\nRequired covariates: ")
  for (confs in must_haves) {
    if (exists(confs, envir = .GlobalEnv)) {
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "use_gp") {
      message("You are missing a 'use_gp' argument in your config. Defaulting it to TRUE")
      use_gp <<- TRUE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "use_stacking_covs") {
      message("You are missing a 'use_stacking_covs' argument in your config. Defaulting it to TRUE")
      use_stacking_covs <<- TRUE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "use_raw_covs") {
      message("You are missing a 'use_raw_covs' argument in your config. Defaulting it to FALSE")
      use_raw_covs <<- FALSE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "fit_with_tmb") {
      message("You are missing a 'fit_with_tmb' argument in your config. Defaulting it to FALSE")
      fit_with_tmb <<- FALSE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "gbd_fixed_effects_measures") {
      message("You are missing a 'gbd_fixed_effects_measures' argument in your config. Defaulting it to 'covariate' for all elements of gbd_fixed_effects")
      gbd_fixed_effects_measures <<- paste(rep("covariate", length(strsplit(gbd_fixed_effects, split = " \\+ ")[[1]])), collapse = " + ")
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "gbd_fixed_effects_age") {
      message("You are missing a 'gbd_fixed_effects_age' argument in your config. Defaulting to '2 3 4 5'")
      gbd_fixed_effects_age <<- "2 3 4 5"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "z_list") {
      message("You are missing a 'z_list' argument in your config. Defaulting it to 0")
      z_list <<- 0
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "zcol") {
      message("You are missing a 'zcol' argument in your config. Defaulting it to z_column_default_blank")
      zcol <<- "z_column_default_blank"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "summstats") {
      message("You are missing a 'summstats' argument in your config. Defaulting to c('mean','lower','upper','cirange')")
      summstats <<- c("mean", "lower", "upper", "cirange")
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "scale_gaussian_variance_N") {
      message("You are missing a 'scale_gaussian_variance_N' argument in your config. Defaulting to TRUE")
      scale_gaussian_variance_N <<- TRUE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "nugget_prior") {
      message("You are missing a 'nugget_prior' argument in your config. Defaulting to 'list(prior = 'loggamma', param = c(2, 1))'")
      nugget_prior <<- "list(prior = 'loggamma', param = c(2, 1))"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "ctry_re_prior") {
      message("You are missing a 'ctry_re_prior' argument in your config. Defaulting to 'list(prior = 'loggamma', param = c(2, 1))'")
      ctry_re_prior <<- "list(prior = 'loggamma', param = c(2, 1))"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "use_nid_res") {
      message("You are missing a 'use_nid_res' argument in your config. Defaulting to FALSE")
      use_nid_res <<- FALSE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "rho_prior") {
      message("You are missing a 'rho_prior' argument in your config. Defaulting to 'list(prior = 'normal', param = c(0, 0.1502314))'")
      rho_prior <<- "list(prior = 'normal', param = c(0, 1/(2.58^2)))"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "use_s2_mesh") {
      message("You are missing a 'use_s2_mesh' argument in your config. Defaulting to FALSE")
      use_s2_mesh <<- FALSE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "s2_mesh_params") {
      message("You are missing a 's2_mesh_params' argument in your config. Defaulting to c(50, 500, 1000)")
      s2_mesh_params <<- "c(25, 500, 1000)"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "modeling_shapefile_version") {
      message("You are missing a 'modeling_shapefile_version' argument in your config. Defaulting to 'current'")
      modeling_shapefile_version <<- "current"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "raking_shapefile_version") {
      message("You are missing a 'raking_shapefile_version' argument in your config. Defaulting to 'current'")
      raking_shapefile_version <<- "current"
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "subnational_raking") {
      message("You are missing a 'subnational_raking' argument in your config. Defaulting to TRUE")
      subnational_raking <<- TRUE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "check_cov_pixelcount") {
      message("You are missing a 'check_cov_pixelcount' argument in your config. Defaulting to FALSE")
      check_cov_pixelcount <<- FALSE
      message(paste0("  ", confs, ": ", eval(confs)))
    } else if (confs == "memory") {
      message("You are missing a 'memory' argument in your config. Defaulting to 10G")
      memory <<- 10
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "singularity_version") {
      message("You are missing a 'singularity_version' argument in your config. Defaulting to 'default'")
      singularity_version <<- "default"
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "queue") {
      message("You are missing a 'queue' argument in your config. Defaulting to 'long.q', unless you have use_geos_nodes to TRUE, which will override this to geospatial.q")
      queue <<- "long.q"
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "run_time") {
      message("You are missing a 'run_time' argument in your config. Defaulting to 16 days ('16:00:00:00')")
      run_time <<- "16:00:00:00"
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "countries_not_to_rake") {
      message("You are missing a 'countries_not_to_rake' argument in your config. Defaulting to ESH+GUF")
      countries_not_to_rake <<- "ESH+GUF"
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "countries_not_to_subnat_rake") {
      message("You are missing a 'countries_not_to_subnat_rake' argument in your config. Defaulting to PHL+NGA+PAK+ETH+KEN")
      countries_not_to_subnat_rake <<- "PHL+NGA+PAK+ETH+KEN"
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "rake_countries") {
      message("You are missing a 'rake_countries' argument in your config. Defaulting to TRUE")
      rake_countries <<- TRUE
      message(paste0("  ", confs, ": ", get(confs)))
    } else if (confs == "spde_prior") {
      message("You are missing a 'spde_prior' argument in your config. Defaulting to 'list(type='pc')'")
      spde_prior <<- "list(type='pc')"
      message(paste0('  ', confs, ': ', get(confs)))
    } else {
      stop(paste0(confs, " is missing, add it to your config"))
    }
  }

  ## Test for subnational random effect (only supporting a single country for now)
  if (exists("use_subnat_res", envir = .GlobalEnv)) {
    stopifnot(exists("subnat_country_to_get", envir = .GlobalEnv))
    stopifnot(length(subnat_country_to_get) == 1)
  } else {
    use_subnat_res <<- FALSE
    subnat_country_to_get <<- FALSE
  }

  message("\nAdditional covariates: ")
  extras <- config$V1[!(config$V1 %in% must_haves)]
  for (extra in extras) message(paste0("  ", extra, ": ", get(extra)))

  ## print out shapefile info
  m.sf.info <- detect_adm_shapefile_date_type(shpfile_path = get_admin_shapefile(version = modeling_shapefile_version))
  r.sf.info <- detect_adm_shapefile_date_type(shpfile_path = get_admin_shapefile(version = modeling_shapefile_version))
  message("\n\n\nSHAPEFILE VERSION INFORMATION: ")
  message(sprintf("\n--MODELING SHAPEFILE VERSION: %s -- which contains %s codes", m.sf.info$shpfile_date, toupper(m.sf.info$shpfile_type)))
  message(sprintf("\n--RAKING SHAPEFILE VERSION:   %s -- which contains %s codes\n", r.sf.info$shpfile_date, toupper(r.sf.info$shpfile_type)))
}
