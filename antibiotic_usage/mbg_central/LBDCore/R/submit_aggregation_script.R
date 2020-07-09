
#' @title Submit Aggregation Script
#' @description This origianlly was in the post_estimation_functions.R script, but was moved
#' to misc_functions.R since all of the other functions which construct qsub
#' commands are here and it needed to be updated to take arguments for
#' Singularity which requires some helper functions that are also defined in
#' misc_functions.R.
#'
#' Constructs a qsub string and executes it
#'
#' @note DOCUMENTATION NEEDS TO BE UPDATED!!
#'
#' @param code Name of script, with relative path if desired.
#'
#' @param code_path Full path to R script. Overrides \code{code} and \code{script_dir}
#'
#' @param cores Number of threads
#'
#' @param memory RAM to be reserved, in GBs
#'
#'
#' @param geo_nodes If TRUE, your job will be submitted to the geos (LBD)
#'   cluster, if FALSE, it will be submitted to the prod cluster. Note that if
#'   using the 'proj' argument, make sure to use project name which is valid on
#'   the cluster you are submitting to. [default = FALSE]
#'
#' @param use_c2_nodes If TRUE, your job will be submitted to the C2 nodes on
#'   the prod cluster, if FALSE, the C2 nodes are not specified. Note that if
#'   FALSE, your job may land on a node with much less memory or your node may
#'   still land on a C2 node anyway. If both the 'use_c2_nodes' and 'geo_nodes'
#'   arguments are set to TRUE, then the code will issue a warning and default
#'   to the geos nodes. [default = FALSE]
#'
#' @param proj Can pass in a project name to submit your job under. If default
#'   and the 'geo_nodes' argument is left as its default of 'FALSE', jobs
#'   will be submitted to the prod cluster under the default project
#'   'proj_geospatial'. If default and with 'geos_nodes = TRUE', jobs will be
#'   submitted to the geos (LBD) nodes under the default project
#'   'proj_geo_nodes'. If a project name is passed in for 'proj' the job will
#'   be submitted under that project. Note that this function does not check for
#'   valid project names since these are likely to change often and likely
#'   valid project names are different on each cluster. [default = NULL]
#'
#' @param queue Queue to be used on the fair cluster.
#'
#' @param run_time Run-time to be used on the fair cluster.
#'
#' @param singularity Launch R from a Singularity image. The default is
#   'default' indicating that you wish to launch a Singularity container from
#'   the default image. You may also provide a string which can be either a complete
#'   path to a Singularity image that is not located at the default image
#'   location, or just the name of the Singularity image that is assumed located
#'   at the default image location. NULL is also accepted, which will launch R
#'   using the default R installation on the geos or prod nodes, but this is
#'   no longer recommended and will likely be deprecated at some point in the
#'   future.
#'
#'   If 'default' is chosen, the default image is defined in the shell script
#'   executed by this R script ('shell_sing.sh') so that no R code need be
#'   updated when the default image is updated. Different versions of a
#'   Singularity image or test versions may be specified by providing the name
#'   or path of the image. Currently, all standard images for LBD are kept at
#'   the default location of /share/singularity-images/lbd.
#'   [default = 'default']
#'
#' @param singularity_opts pass in a named list of environmental variables.
#'   \code{qsub_sing_envs} will check that the names of the list members passed
#'   in match the environmental variables that the shell_sing.sh script knows
#'   about: 'SET_OMP_THREADS' and/or 'SET_MKL_THREADS'. Passing in other
#'   environmental names in the list will result in an error. If this is left
#'   as 'NULL' and a Singularity image is used, SET_OMP_THREADS and
#'   SET_MKL_THREADS will remain unset and the shell_sing.sh script will use
#'   the default setting of SET_OMP_THREADS=1 and SET_MKL_THREADS={max_threads}
#'   (see shell_sing.sh comments). For example SET_OMP_THREADS=1 and
#'   SET_MKL_THREADS=4 can be achieved by passing in
#'     \code{envs = list(SET_OMP_THREADS=1, SET_MKL_THREADS=4)}
#'   [default = list(SET_OMP_THREADS = cores, SET_MKL_THREADS = cores)]
#'
#' @param modeling_shapefile_version character string specifying which shapefile version was used in modeling
#'
#' @param raking_shapefile_version character string specifying which shapefile version was used in raking
#'
#' @param cores specify number of cores to use, defaults to NULL. If this is provided by the user, it is used to assign resources in get_resources
#'
#' @export
#'
submit_aggregation_script <- function(indicator,
                                      indicator_group,
                                      code_path = NULL,
                                      proj = NULL,
                                      run_date,
                                      raked,
                                      pop_measure,
                                      overwrite,
                                      ages,
                                      holdouts,
                                      regions,
                                      corerepo = core_repo,
                                      log_dir,
                                      geo_nodes = FALSE,
                                      use_c2_nodes = FALSE,
                                      queue = NULL,
                                      run_time = NULL,
                                      priority = 0,
                                      cores = 8,
                                      slots = cores,
                                      memory = 20,
                                      singularity = singularity_version,
                                      singularity_opts = list(SET_OMP_THREADS = cores, SET_MKL_THREADS = cores),
                                      modeling_shapefile_version = "current",
                                      raking_shapefile_version = "current",
                                      submit_qsubs = TRUE) {

  # Define project first (necessary to validate node options)
  proj <- get_project(proj, use_geo_nodes = geo_nodes)

  # Validate arguments
  validate_singularity_options(singularity, singularity_opts)
  validate_node_option(geo_nodes, use_c2_nodes, proj)

  # Create sharedir (TODO is this necessary?)
  sharedir <- get_model_output_dir(indicator_group, indicator, run_date)
  dir.create(sharedir, showWarnings = FALSE)

  # Determine where stdout and stderr files will go
  output_err <- setup_log_location(log_dir, user, indic, ig, rd)
  output_log_dir <- output_err[[1]]
  error_log_dir <- output_err[[2]]

  # Define remaining job attributes

  run_time <- get_run_time(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
  queue <- get_queue(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)

  shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_sing.sh")
  sing_image <- get_singularity(image = singularity)
  singularity_str <- qsub_sing_envs("", singularity_opts, sing_image)
  # resources are all the -l qsub arguments
  if (!is.null(cores) & !is.null(slots)) warning("Slots and cores are both specified, cores will be used to assign resources")
  if (is.null(cores)) cores <- slots
  resources <- get_resources(use_geo_nodes = geo_nodes, cores = cores, ram_gb = memory, runtime = run_time)

  code <- path_join(corerepo, "mbg_central", "share_scripts", "aggregate_results.R")

  # If code_path is not NULL, then override `code`
  if (!is.null(code_path)) {
    code <- code_path
  }

  qsubs_to_make <- expand.grid(regions, holdouts, ages, raked)

  aggregation_qsubs <- make_qsubs_aggregation(
    qsubs_to_make, error_log_dir, output_log_dir, proj, resources, singularity_str, queue, priority, slots, shell, code,
    indicator, indicator_group, run_date, pop_measure, overwrite, corerepo, raking_shapefile_version, modeling_shapefile_version
  )

  if (submit_qsubs) {
    for (qsub in aggregation_qsubs) {
      system(qsub)
    }
  }
  return(aggregation_qsubs)
}
