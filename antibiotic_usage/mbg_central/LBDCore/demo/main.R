

# (1) Preamble: Set up libraries ------------------------------------------


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

## Load colorout for nice colored terminal output
library(colorout)

## Define the path to the Singularity image to be used for our jobs
sing_image <- paste0(
  "/ihme/singularity-images/lbd/",
  "testing_INLA_builds/lbd_rpkgs3.6.1gcc9mkl",
  "rstudioserver1.2.1555_v5.simg"
)

# (2) Prep Pipeline Object ------------------------------------------------



## Initial parameters
core_repo <- sprintf(
  "/share/code/geospatial/%s/lbd_core/",
  Sys.info()["user"]
)
ig <- "training"
indic <- "tr_had_diarrhea"
config <- paste0(core_repo, "training/config_training_pipeline.csv")
covs <- paste0(core_repo, "training/covs_training.csv")

Pipeline_Class <- MBGPipeline$new(
  core_repo = core_repo,
  indicator_group = ig,
  indicator = indic,
  config_file = config,
  covs_file = covs
)
Pipeline_Class$setup_conf(
  push_to_global_env = FALSE,
  run_tests = FALSE
)
Pipeline_Class$setup_rundate(
  prefix = paste0(Sys.info()["user"], "_"),
  suffix = "_test"
)
Pipeline_Class$make_holdouts()
Pipeline_Class$create_loopvars()



#### In order to overload config, one can change the `config_file`
#### input to a new config file CSV for e.g.:
#### "/share/code/geospatial/sadatnfs/training/config_training.csv"
#### And then rerun the `setup_conf` method.

#### This also works with the MBGDag below!



# (3) Make MBG DAG per region ---------------------------------------------




### NOTE: foreach with parallel is ideal in order to
### have perfectly parallel DAGs being tracked simultaneously

## Stop any previously lingering clusters
stopImplicitCluster()

## Create the number of threads equal to the number of DAGs we will track
registerDoParallel(cores = nrow(Pipeline_Class$loopvars))

## Run each of the DAGs parallelly
full_DAG_graph <- foreach(r = 1:nrow(Pipeline_Class$loopvars)) %dopar% {

  ## r = 1
  loop_reg <- Pipeline_Class$loopvars[r, region]
  loop_age <- Pipeline_Class$loopvars[r, age]
  loop_holdout <- Pipeline_Class$loopvars[r, holdout]

  print(paste0(" ##### ", loop_reg, " ##### "))

  DAG_object <- MBGDag$new(
    pipeline_class = Pipeline_Class,
    reg = loop_reg,
    age = loop_age,
    holdout = loop_holdout
  )

  DAG_object$create_node(
    nodename = "data_prep",
    jobscript = paste0(
      core_repo, "mbg_central/LBDCore/demo/01_PM_Data_Prepping.R"
    ),
    project = "proj_geo_nodes",
    cores = 1,
    ram_gb = 4,
    runtime = "01:00:00",
    queue = "geospatial.q",
    singularity_version = sing_image
  )

  DAG_object$create_node(
    nodename = "stacking",
    jobscript = paste0(
      core_repo, "mbg_central/LBDCore/demo/02_PM_Stacking.R"
    ),
    project = "proj_geo_nodes",
    cores = 1,
    ram_gb = 4,
    runtime = "00:10:00",
    queue = "geospatial.q",
    hold_job = "previous",
    singularity_version = sing_image
  )


  DAG_object$create_node(
    nodename = "prep_for_INLA",
    jobscript = paste0(
      core_repo, "mbg_central/LBDCore/demo/03_PM_PrepForFitting.R"
    ),
    project = "proj_geo_nodes",
    cores = 1,
    ram_gb = 4,
    runtime = "00:10:00",
    queue = "geospatial.q",
    hold_job = "previous",
    singularity_version = sing_image
  )



  DAG_object$create_node(
    nodename = "MBG_fitting",
    jobscript = paste0(
      core_repo, "mbg_central/LBDCore/demo/04_PM_MBGFitting.R"
    ),
    project = "proj_geo_nodes",
    cores = 3,
    ram_gb = 5,
    runtime = "00:20:00",
    queue = "geospatial.q",
    hold_job = "previous",
    singularity_version = sing_image
  )

  DAG_object$create_node(
    nodename = "MBG_predict",
    jobscript = paste0(
      core_repo, "mbg_central/LBDCore/demo/05_PM_MBGPredict.R"
    ),
    project = "proj_geo_nodes",
    cores = 1,
    ram_gb = 5,
    runtime = "00:20:00",
    queue = "geospatial.q",
    hold_job = "previous",
    singularity_version = sing_image
  )


  ## Save DAG
  print(
    paste0(
      "Saving DAG for row: ", r, " in LV table to:",
      DAG_object$get_prerun_image_path(file_format = ".rds")
    )
  )
  saveRDS(object = DAG_object, file = DAG_object$img_path)


  ### Look at DAG
  print(DAG_object$DAG)

  ## Submit everything in the DAG, and wait until finish
  DAG_object$submit_jobs()
  Sys.sleep(5)
  DAG_object$wait_on_node()
  saveRDS(object = DAG_object, file = DAG_object$img_path)


  ## Return the list of region specific DAGs
  return(DAG_object)
}

stopImplicitCluster()




# (4) Extra things one can do ---------------------------------------------




#### If we want to load the DAG:
# readRDS("/share/geospatial/mbg/training/tr_had_diarrhea/model_image_history/DAG_2019_05_09_52_53_bin0_ken_0.rds")

#### : If we want to remove one of the node Rdata files
# file.remove(DAG_object$get_node_Rdata_path(node = "data_prep"))


## Remove nodes with:
# DAG_object$remove_node("stacking")

## One can submit single nodes and track them as follows:
# ## Submit data_prep and update DAG
# DAG_object$submit_jobs(nodename = "data_prep")
# DAG_object$wait_on_node("data_prep")
# saveRDS(object = DAG_object, file = DAG_object$img_path)
