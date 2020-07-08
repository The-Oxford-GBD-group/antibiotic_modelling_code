

# Preamble ----------------------------------------------------------------


print(Sys.time())

## Point to personal directory (create if needed)
personal_lib <- sprintf(
  "~/R/x86_64-pc-linux-gnu-library/%s.%sgeo/",
  R.Version()$major, R.Version()$minor
)
Sys.setenv(R_LIBS_USER = personal_lib)
if (!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)

## Set up .libPaths()
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

## Set up MKLROOT directory (needed if using RStudio)
Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")

## Load the LBDCore library
## The `suppressMessages()` is there just to remove all the intermediate
## packages' loading messages.
suppressMessages(library(LBDCore))

## Setup preambles
pipeline_preamble(headnode = FALSE)



# Predict MBG -------------------------------------------------------------



## Run predict_mbg on chunks of 50 samples (to avoid memory issues)
message("Making predictions in 50 draw chunks.")

max_chunk <- 50
samples <- as.numeric(DAG_obj$pipeline$config_list$samples)

## Create vector of chunk sizes
chunks <- rep(max_chunk, samples %/% max_chunk)
if (samples %% max_chunk > 0) chunks <- c(chunks, samples %% max_chunk)
pm <- lapply(chunks, function(samp) {
  if (DAG_obj$pipeline$config_list$fit_with_tmb == FALSE) {
    predict_mbg(
      res_fit = model_fit,
      cs_df = cs_df,
      mesh_s = mesh_s,
      mesh_t = mesh_t,
      cov_list = cov_list,
      samples = samp,
      simple_raster = simple_raster,
      transform = DAG_obj$pipeline$config_list$transform,
      coefs.sum1 = DAG_obj$pipeline$config_list$coefs_sum1,
      nperiod = length(DAG_obj$pipeline$config_list$year_list),
      pred_gp = DAG_obj$pipeline$config_list$use_gp,
      shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version
    )[[3]]
  } else {
    predict_mbg_tmb(
      samples = samp,
      seed = NULL,
      tmb_input_stack = input_data,
      model_fit_object = model_fit,
      fes = all_fixed_effects, # TODO use input_data or model_fit object for this (in case its changed due to checks)
      sr = simple_raster,
      yl = DAG_obj$pipeline$config_list$year_list,
      zl = DAG_obj$pipeline$config_list$z_list,
      covs_list = cov_list,
      clamp_covs = clamp_covs
    ) # TODO ADD CONFIG
  }
})



# Finish Up ---------------------------------------------------------------



# if z dimension has more than one level, then save each z as a different indicator
if (length(DAG_obj$pipeline$config_list$z_list) > 1) {

  # reorder pm list, right now its z within each chunk. rbind all z's together
  for (z in DAG_obj$pipeline$config_list$z_list) { # z_list must be integers starting with 1
    if (length(chunks) > 1) {
      for (ch in 2:length(chunks)) {
        pm[[1]][[z]] <- cbind(pm[[1]][[z]], pm[[ch]][[z]])
      }
    }
  }
  pm <- pm[[1]] # pm is now a list of cell_preds by z


  # loop over z and save as an indicator each one
  orig_indic <- indicator ## indicator from argparse

  message("Wrapping up")

  for (z in z_list) {
    cptmp <- pm[[z]]

    indicator <- sprintf("%s_%s%i", orig_indic, zcol, z) # new indicator name
    pathaddin <- paste0("_bin", z, "_", reg, "_", holdout) # new pathaddin
    outputdir <- sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/",
      indicator_group, indicator, run_date
    ) # new outputdir
    dir.create(outputdir)
    message(sprintf("New indicator: %s", indicator))

    # make a mean raster
    mean_ras <- insertRaster(
      simple_raster,
      matrix(rowMeans(cptmp),
        ncol = max(period_map$period)
      )
    )
    sd_ras <- insertRaster(
      simple_raster,
      matrix(rowSds(cptmp), ncol = max(period_map$period))
    )

    # save z specific objects
    writeRaster(
      mean_ras,
      file = paste0(outputdir, "/", indicator, "_prediction_eb", pathaddin),
      overwrite = TRUE
    )

    save(
      cptmp,
      file = paste0(
        outputdir, "/",
        indicator, "_cell_draws_eb", pathaddin, ".RData"
      ),
      compress = TRUE
    )

    pdf(paste0(outputdir, "mean_raster", pathaddin, ".pdf"))
    plot(mean_ras, main = "mean", maxpixel = 1e6)
    plot(sd_ras, main = "sd", maxpixel = 1e6)
    dev.off()

    rm(cptmp)
  }

  ## Reset the constants back
  indicator <- orig_indic

  # save training data
  write.csv(
    df,
    file = paste0(
      DAG_obj$pipeline$outputdir, "/", indicator,
      "_trainingdata", DAG_obj$pathaddin, ".csv"
    ),
    row.names = FALSE
  )

  message("done saving indicator-specific outputs by z")
} else {
  ## if no z columns (most peoples cases) ##
  ## Make cell preds and a mean raster
  cell_pred <- do.call(cbind, pm)
  mean_ras <- insertRaster(
    simple_raster,
    matrix(rowMeans(cell_pred),
      ncol = max(period_map$period)
    )
  )



  message("Wrapping up")
  save_mbg_preds(
    config = config,
    time_stamp = TRUE,
    run_date = run_date,
    mean_ras = mean_ras,
    sd_ras = NULL,
    res_fit = model_fit,
    cell_pred = cell_pred,
    df = df,
    pathaddin = DAG_obj$pathaddin
  )


  # plot the mean raster
  pdf(
    paste0(
      DAG_obj$pipeline$outputdir,
      "/mean_rasterXX", DAG_obj$pathaddin, ".pdf"
    )
  )
  plot(mean_ras, maxpixel = 1e6)
  dev.off()
}


# Postamble ---------------------------------------------------------------


pipeline_postamble()
q("no")
