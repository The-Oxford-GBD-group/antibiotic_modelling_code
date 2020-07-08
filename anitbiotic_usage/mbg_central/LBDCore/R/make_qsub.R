#' @title Make qsub
#'
#' @description Constructs a qsub string and returns it
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
#'   [default = NULL]
#'
#' @return Returns a qsub string
#'
#' @export
make_qsub <- function(user = Sys.info()["user"],
                      code,
                      cores = slots,
                      memory = 100,
                      proj = NULL,
                      ig = indicator_group,
                      indic = indicator,
                      reg = "test",
                      age = 0,
                      vallevel = "",
                      hard_set_sbh_wgt = TRUE,
                      pct_sbh_wgt = 100,
                      rd = run_date,
                      log_location = "sgeoutput",
                      saveimage = FALSE,
                      test = FALSE,
                      holdout = 0,
                      use_base_covariates = FALSE,
                      test_pv_on_child_models = TRUE,
                      constrain_children_0_inf = FALSE,
                      child_cv_folds = 5,
                      fit_stack_cv = TRUE,
                      shell = "r_shell.sh",
                      modeltype = "full",
                      corerepo = core_repo,
                      geo_nodes = FALSE,
                      use_c2_nodes = FALSE,
                      singularity = singularity_version,
                      singularity_opts = NULL) {
  if (test) t <- 1 else t <- 0
  if (use_base_covariates) b <- 1 else b <- 0
  if (test_pv_on_child_models) pvc <- 1 else pvc <- 0
  if (constrain_children_0_inf) cc0i <- 1 else cc0i <- 0
  if (fit_stack_cv) fscv <- 1 else fscv <- 0
  if (hard_set_sbh_wgt) hssw <- 1 else hssw <- 0

  if (saveimage == TRUE) save.image(paste0("/share/geospatial/mbg/", ig, "/", indic, "/model_image_history/pre_run_tempimage_", rd, "_bin", age, "_", reg, "_", holdout, ".RData"))

  # dir.create(sprintf('%s/output/%s',sharedir,rd))
  dir.create(paste0("/share/geospatial/mbg/", ig, "/", indic, "/output/", rd), showWarnings = F)

  sharedir <- sprintf("/share/geospatial/mbg/%s/%s", ig, indic)

  if (log_location == "sgeoutput") {
    logloc <- sprintf("/share/temp/sgeoutput/%s", user)
  }
  if (log_location == "sharedir") {
    logloc <- sprintf("%s/output/%s", sharedir, rd)
    dir.create(sprintf("%s/errors", logloc), showWarnings = F)
    dir.create(sprintf("%s/output", logloc), showWarnings = F)
  }

  # Submit to geo or prod nodes with different default projects.
  if (geo_nodes) {
    # The geo nodes have default project 'proj_geo_nodes' if the 'proj' argument
    # is left as NULL and also require the '-l geos_node=TRUE' complex for UGE
    if (is.null(proj)) proj <- "proj_geo_nodes"
    node.flag <- "-l geos_node=TRUE"
  } else {
    # The prod nodes have default project 'proj_geospatial' if the 'proj'
    # argument is left as NULL
    if (is.null(proj)) proj <- "proj_geospatial"
    if (use_c2_nodes) node.flag <- "-q all.q@@c2-nodes" else node.flag <- ""
  }

  # At least give a warning if both 'geo_nodes' and 'use_c2_nodes' are requested
  if (geo_nodes & use_c2_nodes) {
    message("WARNING: Both 'geo_nodes' and 'use_c2_nodes' arguments were set to TRUE")
    message(paste0("         Submitting job to LBD nodes under project: '", proj, "'"))
  }

  # If the user has passed in options for the Singularity container in the
  # 'singularity_opts' argument, but the 'singularity' argument is 'NULL' exit
  # with an error.
  if (is.null(singularity) & !is.null(singularity_opts)) {
    message("'singularity' argument is 'NULL' but options passed in for 'singularity_opts'")
    stop("Exiting!")
  }

  # If the script is to be run with R from a Singularity container, point to
  # the shell script to launch the container. Users can provide the 'default'
  # keyword to get the default Singulariy image, just the name of the image
  # located at the default path, or the full path to the image.
  if (!is.null(singularity)) {
    shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_sing.sh")
    # Determine which Singularity image to use:
    sing_image <- get_singularity(image = singularity)
  } else {
    # if not, fire up the appropriate version of R depending on the cluster node
    shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_cluster.sh")
  }

  # Piece together lengthy `qsub` command
  qsub <- paste0(
    "qsub",
    " -e ", logloc, "/errors",
    " -o ", logloc, "/output",
    " -cwd -l mem_free=", memory, "G",
    " -pe multi_slot ", cores,
    " -P ", proj, " ", node.flag
  )

  # If a Singularity image is being used, pass the name of the image from
  # `get_singularity` as well as any other environmentals the user asked for
  # from the 'singularity_opts' argument to the qsub job
  if (!is.null(singularity)) {
    qsub <- qsub_sing_envs(
      qsub, singularity_opts,
      sing_image
    )
  }

  # append the rest of the qsub command
  qsub <- paste0(
    qsub,
    " -N job_", reg, "_", age, "_", holdout, vallevel,
    " ", ig, "/", shell, " ", ig, "/", code, ".R"
  )
  qsub <- paste(qsub,
    reg, age, rd, t, holdout, indic, ig, b, pvc, cc0i,
    child_cv_folds, fscv, modeltype, vallevel, pct_sbh_wgt, hssw,
    sep = " "
  )

  return(qsub)
}
