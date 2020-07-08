#' @title MBG DAG Class for Parallel Model.
#' @docType class
#' @keywords MBG parallel_model
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} with the DAG for the parallel model.
#'
#' @format \code{\link{R6Class}} object.
#'
#'
#' @field pipeline_class An \code{MBGPipeline} object
#' @field dag_hash A 5 character random string used to hash the nodes.
#' The hash is randomly generated if not supplied by user.
#' @field reg Region
#' @field age Age
#' @field holdout Holdout fold
#' @field test Test mode?
#' @field tmpdir Path to temporary directory to start job markers in.
#' Default: \code{"~/.mbgdir"}.
#'
#'
#' @section DAG Table:
#' The DAG table will consist of the following information about the
#' nodes (the jobs):
#' \itemize{
#' \item{\code{nodename}}: Name of node in DAG
#'
#' \item{\code{node_counter}: Auto-incrementing counter specifying the
#' node number in the graph
#' }
#'
#' \item{\code{jobscript}: Path to the R script being run
#' }
#'
#' \item{\code{job_name}: Name of job
#' }
#'
#' \item{\code{project}: Cluster project
#' }
#'
#' \item{\code{queue}: Queue
#' }
#'
#' \item{\code{fthread}: Number of CPUs requested
#' }
#'
#' \item{\code{m_mem_free}: Amount of RAM requested
#' }
#'
#' \item{\code{h_rt}: Runtime requested
#' }
#'
#' \item{\code{qsub_str}: The qsub string
#' }
#'
#' \item{\code{status}: Job status
#' }
#'
#' \item{\code{job_id}: Job ID (after job has been submitted)
#' }
#'
#' }
#'
#'
#' @section Methods:
#' \describe{
#' \item{\code{print}}{
#'   Prints the DAG info
#' }
#'
#'
#' \item{\code{argparse_append}}{
#'  Prepare a parameter to be passed as argparse input into jobs. Return just the value if \code{use_argparse = FALSE}
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{val}: Argument value
#'     }
#'     \item{\code{val_lab}: Argparse field, parsed as: \code{"-- val_lab val"}
#'     }
#'     \item{\code{use_argparse}: Use argparse to create arguments? Default: \code{FALSE}.
#'      }
#'   }
#' }
#'
#'
#' \item{\code{clean_dag}}{
#'  Empties the DAG object
#' }
#'
#' \item{\code{get_prerun_image_path}}{
#'  Evaluate a config parameter
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{ig}: Indicator group, from the pipeline object.
#'     }
#'     \item{\code{indic}: Indicator, from the pipeline object.
#'     }
#'     \item{\code{rd}: Run date, from the pipeline object.
#'     }
#'     \item{\code{file_format}: File format suffix. Default: \code{".rds"}
#'     }
#'   }
#' }
#'
#' \item{\code{create_node}}{
#'  Create a node (job) for the DAG
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{nodename}: Name of node.
#'     }
#'     \item{\code{jobscript}: Path to R script.
#'     }
#'     \item{\code{project}: Cluster project. Default: \code{"proj_geo_nodes"}.
#'     }
#'     \item{\code{cores}: Number of threads. Default: 1.
#'     }
#'     \item{\code{ram_gb}: Amount of RAM. Default: 5 (5 GB).
#'     }
#'     \item{\code{runtime}: Run-time. Default: \code{"01:00:00"} (1 hour).
#'     }
#'     \item{\code{queue}: Queue. Default: \code{"geospatial.q"}.
#'     }
#'     \item{\code{hold_job}: Node to hold on. Default: \code{"previous"},
#'      which will create a hold on the previous job. Alternatively, the node
#'      on which to hold can also be specified.
#'     }
#'     \item{\code{log_location}: Location to store logs to feed into
#'      \code{setup_log_location}. Default: \code{"sharedir"}
#'     }
#'     \item{\code{singularity_version}: Singularity image (version or absolute
#'     path to image). Default: \code{"default"}
#'     }
#'     \item{\code{singularity_opts}: A list with values for
#'     \code{SET_OMP_THREADS} and  \code{SET_MKL_THREADS} to pass in OMP and
#'     MKL threads. Defaults to using the same amount of threads for each as
#'     the number of cores.
#'     }
#'     \item{\code{shell_script}: Path to shell script. Note that using
#'     argparse requires a special shell script (\code{shell_sing_argparse.sh}).
#'     }
#'   }
#' }
#'
#' \item{\code{get_dag_value}}{
#'  Get field from the DAG given the nodename
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{name}: Node name.
#'     }
#'     \item{\code{counter}: Node counter.
#'     }
#'     \item{\code{field}: Field to get value from.
#'     }
#'   }
#' }
#'
#' \item{\code{get_all_parent_nodes}}{
#'  Get all the parent nodes for a given node name. This is helpful if you want
#' to hold on a whole set of jobs
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{name}: Node name
#'     }
#'   }
#' }
#'
#' \item{\code{update_dag}}{
#'  Update a value of a field in the DAG table for a given node.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{name}: Node name.
#'     }
#'     \item{\code{counter}: Node counter.
#'     }
#'     \item{\code{field_to_update}: Field to update value of.
#'     }
#'     \item{\code{update_value}: The value to update to.
#'     }
#'   }
#' }
#'
#' \item{\code{submit_jobs}}{
#'  Submit jobs of the DAG given the node name(s) provided.
#'  If the node name is left to \code{NULL}, then all of the jobs are submitted.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{nodename}: Name(s) of node to submit for job.
#'     }
#'   }
#' }
#'
#' \item{\code{get_node_Rdata_path}}{
#'  Get the file path of the node with the environments saved out.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{name}: Node name.
#'     }
#'     \item{\code{indic}: Indicator.
#'     }
#'     \item{\code{ig}: Indicator Group.
#'     }
#'     \item{\code{rd}: Run date.
#'     }
#'     \item{\code{age}: Age bin.
#'     }
#'     \item{\code{reg}: Region.
#'     }
#'     \item{\code{holdout}: Holdout fold.
#'     }
#'   }
#' }
#'
#' \item{\code{remove_node}}{
#'  Drop node from the DAG. Note that this does not reset the node counter!
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{node_erase}: Node name(s) to erase.
#'     }
#'   }
#' }
#'
#' \item{\code{wait_on_job}}{
#'  Generic method to check on job IDs on whether the job finished or not.
#'  Job tracking validation is done on whether the start and end job marker
#'  files are successfully written or not at different stages of the job.
#'  In order to tag a job as successfully finished, the job has to not exist
#'  in qstat, the start file must not exist, and the end file must exist.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{job_ids}: Job IDs to track.
#'     }
#'     \item{\code{sleeptime}: Seconds to sleep until pinging qstat again.
#'     }
#'     \item{\code{tmpdir}: Temporary directory to track job markers.
#'     }
#'   }
#' }
#'
#' \item{\code{wait_on_node}}{
#'  A wrapper on \code{wait_on_job} to take jobs in the DAG, and tag status.
#'  Multiple jobs are tracked parallelly using \code{foreach}.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{nodename}: Node name(s) to track and tag status.
#'     }
#'     \item{\code{...}: All other arguments to pas to \code{wait_on_job}.
#'     }
#'   }
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#' ## Create a Pipeline object apriori
#'
#' ## Create a new MBGDag
#' DAG_object <- MBGDag$new(
#'   pipeline_class = Pipeline_Class,
#'   reg = test_reg,
#'   age = test_age,
#'   holdout = test_holdout, dag_hash = "vmpel"
#' )
#'
#' ## Create a node
#' DAG_object$create_node(
#'   nodename = "data_prep",
#'   jobscript = paste0(
#'     "/ihme/code/geospatial/sadatnfs/lbd_core/pipeline/scripts/01_PM_Data_Prepping.R"
#'   ),
#'   project = "proj_geo_nodes",
#'   cores = 1,
#'   ram_gb = 4,
#'   runtime = "00:10:00",
#'   queue = "geospatial.q",
#'   singularity_version = paste0(
#'     "/ihme/singularity-images/lbd/",
#'     "testing_INLA_builds/lbd_rpkgs3.6.0gcc9mkl",
#'     "rstudioserver1.2.1511_v4.simg"
#'   )
#' )
#'
#'
#' ## Save DAG
#' print(
#'   paste0(
#'     "Saving DAG for row: ", test_row, " in LV table to:",
#'     DAG_object$get_prerun_image_path(file_format = ".rds")
#'   )
#' )
#' saveRDS(object = DAG_object, file = DAG_object$img_path)
#'
#'
#' ### Preview DAG
#' DAG_object$DAG
#'
#' ## Submit all the jobs and track the jobs
#' DAG_object$submit_jobs()
#' DAG_object$wait_on_node()
#' saveRDS(object = DAG_object, file = DAG_object$img_path)
#' }
#'
MBGDag <- R6::R6Class(
  classname = "MBGDag",

  # lock_objects = FALSE is necessary for being able to add more
  # to self after initializing
  lock_objects = FALSE,
  private = list(
    ## Wait for job to finish
    wait_on_job = function(job_ids, sleeptime = 100, tmpdir) {

      # Check if job_id is still in qstat list or not
      while (job_ids %in% system("qstat | awk {'print $1'}",
        intern = TRUE
      )[-2:-1]) {
        print("The following job IDs are still in qstat:")
        print(job_ids)
        print(paste0("Continue to sleep.... It's ", Sys.time()))
        Sys.sleep(sleeptime)
      }

      print("Job IDs not found in qstat. Checking for completion...")
      print(job_ids)

      ## Check in tmpdir on jobs' status
      for (to_check in job_ids) {
        start_file <- paste0(tmpdir, "/", to_check, "_start")
        end_file <- paste0(tmpdir, "/", to_check, "_end")

        if (file.exists(start_file) & file.exists(end_file)) {
          message("Both start and end file exists but job isn't running...")
          return("weird")
        } else if (file.exists(start_file) & !file.exists(end_file)) {
          message("Start file exists but no end file. Job errored out.")
          return("errored")
        } else if (!file.exists(start_file) & !file.exists(end_file)) {
          message("Job never started off to write either file.")
          return("errored")
        } else {
          message("Job successfully finished.")
          return("finished")
        }
      }
    },

    ## Empty DAG dt maker
    empty_dag = function() {
      return(
        data.table(
          "nodename" = character(),
          "node_counter" = integer(),
          "jobscript" = character(),
          "job_name" = character(),
          "hold_on_job" = character(),
          "project" = character(),
          "queue" = character(),
          "fthread" = integer(),
          "m_mem_free" = character(),
          "h_rt" = character(),
          "qsub_str" = character(),
          "status" = character(),
          "job_id" = integer()
        )
      )
    }
  ),
  public = list(
    pipeline_class = NULL,
    dag_hash = NULL,
    reg = NULL,
    age = NULL,
    holdout = NULL,
    test = NULL,
    tmpdir = NULL,
    initialize = function(pipeline_class,
                              reg,
                              age,
                              holdout,
                              test = FALSE,
                              dag_hash = NULL,
                              tmpdir = "~/.mbgdir") {
      self$pipeline <- pipeline_class
      self$reg <- reg
      self$age <- age
      self$holdout <- holdout
      self$test <- test
      self$tmpdir <- tmpdir

      self$node_counter <- 0
      self$DAG <- private$empty_dag()

      if (is.null(dag_hash)) {
        self$dag_hash <- stringi::stri_rand_strings(1, 5)
      } else {
        self$dag_hash <- dag_hash
      }
      self$run_date <- self$pipeline$run_date
      self$pathaddin <- paste0("_bin", age, "_", reg, "_", holdout)
    },



    ## Helper function for argparse
    argparse_append = function(val, val_lab = "", use_argparse = FALSE) {
      if (!use_argparse) {
        return(val)
      }
      return(paste0("--", val_lab, " ", val))
    },


    ## Clean up DAG table
    clean_dag = function() {
      self$node_counter <- 0
      self$DAG <- private$empty_dag()
    },


    # Get the pre-run image file path
    get_prerun_image_path = function(ig = self$pipeline$indicator_group,
                                         indic = self$pipeline$indicator,
                                         rd = self$pipeline$run_date,
                                         file_format = ".rds") {
      self$img_path <- pre_run_image_path(
        indicator_group = ig,
        indicator = indic,
        run_date = rd,
        age = self$age,
        region = self$reg,
        holdout = self$holdout,
        file_format = file_format,
        prefix = "DAG"
      )
      return(self$img_path)
    },



    ## Create a node in the DAG
    create_node = function(nodename,
                               jobscript,
                               project = "proj_geo_nodes",
                               cores = 1,
                               ram_gb = 5,
                               runtime = "01:00:00",
                               queue = "geospatial.q",
                               hold_job = "previous",
                               log_location = "sharedir",
                               singularity_version = "default",
                               singularity_opts = list(
                                 SET_OMP_THREADS = cores,
                                 SET_MKL_THREADS = cores
                               ),
                               shell_script = paste0(
                                 self$pipeline$core_repo,
                                 "/mbg_central/share_scripts/",
                                 "shell_sing_argparse.sh"
                               )) {

      # Add to node counter
      self$node_counter <- self$node_counter + 1

      # Job name
      this_job_name <- paste(
        "job_pmod",
        self$reg,
        self$age,
        self$holdout,
        nodename,
        self$pipeline$run_date,
        self$dag_hash,
        sep = "_"
      )

      # Set up error and output locations
      output_err <- setup_log_location(
        log_location = log_location,
        user = self$pipeline$user,
        indicator = self$pipeline$indicator,
        indicator_group = self$pipeline$indicator_group,
        run_date = self$pipeline$run_date
      )
      self$output_log_dir <- output_err[[1]]
      self$error_log_dir <- output_err[[2]]

      # Set up resources
      resources <- get_resources(
        cores = cores,
        ram_gb = ram_gb,
        runtime = runtime
      )


      ## Job holder
      job_hold_str <- NA
      parent_node <- NA
      if (!is.null(hold_job)) {
        if (self$node_counter <= 1) {
        } else {
          if (hold_job == "previous") {
            job_hold_str <- self$get_dag_value(
              counter = max(self$DAG$node_counter),
              field = "job_name"
            )
            parent_node <- self$get_dag_value(
              counter = max(self$DAG$node_counter),
              field = "nodename"
            )
          } else {
            job_hold_str <- self$get_dag_value(
              name = hold_job,
              field = "job_name"
            )
            parent_node <- self$get_dag_value(
              name = hold_job,
              field = "nodename"
            )
          }
        }
      }

      ## Add node entry to DAG
      self$DAG <- rbindlist(list(
        self$DAG,
        data.table(
          "nodename" = nodename,
          "node_counter" = self$node_counter,
          "jobscript" = jobscript,
          "parent_node" = parent_node,
          "hold_on_job" = job_hold_str,
          "project" = project,
          "queue" = queue,
          "job_name" = this_job_name,
          "fthread" = cores,
          "m_mem_free" = paste0(ram_gb, "G"),
          "h_rt" = runtime
        )
      ), use.names = TRUE, fill = TRUE)


      ## Create qsub string
      node_qsub_string <- generate_qsub_command(
        job_name = this_job_name,
        stderr_log = self$error_log_dir,
        stdout_log = self$output_log_dir,
        hold_jid = job_hold_str,
        project = project,
        resources = resources,
        queue = queue,
        singularity_str = qsub_sing_envs(
          "",
          list(SET_OMP_THREADS = cores, SET_MKL_THREADS = cores),
          singularity_version
        ),
        shell = shell_script,
        code_path = jobscript,
        self$argparse_append(
          self$reg,
          "reg", TRUE
        ),
        self$argparse_append(
          self$age,
          "age", TRUE
        ),
        self$argparse_append(
          self$pipeline$run_date,
          "run_date", TRUE
        ),
        self$argparse_append(
          self$holdout,
          "holdout", TRUE
        ),
        self$argparse_append(
          self$test,
          "test", TRUE
        ),
        self$argparse_append(
          self$pipeline$indicator,
          "indicator", TRUE
        ),
        self$argparse_append(
          self$pipeline$indicator_group,
          "indicator_group", TRUE
        ),
        self$argparse_append(
          nodename,
          "nodename", TRUE
        ),
        self$argparse_append(
          self$dag_hash,
          "dag_hash", TRUE
        )
      )

      ## Update dag with qsub call
      self$update_dag(
        name = nodename,
        field_to_update = "qsub_str",
        update_value = node_qsub_string
      )
      ## End of create_node() ##
    },

    # Get field from the DAG given the nodename
    get_dag_value = function(name = "", counter = 0, field = "") {
      if ((name == "" | is.na(name)) &
        (counter == 0 | is.na(counter))) {
        stop("Both can't be missing")
      }
      if (name != "") {
        return(self$DAG[
          nodename %in% paste0(name),
          get(paste0(field))
        ])
      } else {
        return(self$DAG[
          node_counter %in% counter,
          get(paste0(field))
        ])
      }
    },

    # Get all parent nodes upto the requested node
    get_all_parent_nodes = function(name) {

      ## Get row number of node
      node_rownum <- self$DAG[nodename == name, which = TRUE]

      ## Get all of the parent values
      return(na.omit(self$DAG[1:node_rownum, parent_node]))
    },

    # Update any entries in the DAG dt
    update_dag = function(name = "",
                              counter = 0,
                              field_to_update = "",
                              update_value = "") {
      if ((name == "" | is.na(name)) &
        (counter == 0 | is.na(counter))) {
        stop("Both can't be missing")
      }
      if (name != "") {
        self$DAG[
          nodename %in% paste0(name),
          paste0(field_to_update) := update_value
        ]
      } else {
        self$DAG[
          node_counter %in% counter,
          paste0(field_to_update) := update_value
        ]
      }
    },

    # Submit a job!!
    submit_jobs = function(nodename = NULL) {
      if (is.null(nodename)) {
        print("Submitting all jobs in the DAG")
        node_to_qsub <- unique(self$DAG$nodename)
      } else {
        node_to_qsub <- nodename
      }

      for (nodes in node_to_qsub) {
        returned <- system(self$get_dag_value(
          name = nodes,
          field = "qsub_str"
        ), intern = TRUE)
        message(returned)
        job_id <- as.numeric(stringr::str_match(
          returned,
          "Your job ([0-9]*) "
        )[, 2])

        # Concat the job_ID to job_str data.table
        self$update_dag(
          name = nodes,
          field_to_update = "job_id",
          update_value = job_id
        )

        # Tag status 'submitted'
        self$update_dag(
          name = nodes,
          field_to_update = "status",
          update_value = "submitted"
        )
      }
    },


    ## Get path to image of node
    get_node_Rdata_path = function(node,
                                       indic = self$pipeline$indicator,
                                       ig = self$pipeline$indicator_group,
                                       rd = self$run_date,
                                       age = self$age,
                                       reg = self$reg,
                                       holdout = self$holdout) {
      path_str <- sprintf("DAG_%s_bin%s_%s_%s.RData", node, age, reg, holdout)

      img_path <- path_join(
        get_indicator_dir(
          ig, indic
        ),
        "output",
        rd,
        path_str
      )

      return(img_path)
    },


    ## Drop node from DAG
    remove_node = function(node_erase = "") {
      self$DAG <- self$DAG[!(nodename %in% node_erase)]
    },

    ## Wrap wait_on_job to simplify for node
    wait_on_node = function(nodename = NULL, ...) {
      if (is.null(nodename)) {
        ## Get all the nodes!
        node_to_qsub <- unique(self$DAG$nodename)
      } else {
        node_to_qsub <- nodename
      }


      ### Track jobs. This could have been done parallely
      ### but if we will be tracking multiple DAGs in parallel,
      ### then it's best to not nest those

      tmplist <- foreach(nn = node_to_qsub) %do% {

        # Get job state on finish (successful or otherwise)
        job_state <- private$wait_on_job(
          job_ids = self$get_dag_value(
            name = nn,
            field = "job_id"
          ),
          tmpdir = self$tmpdir,
          ...
        )

        print(paste0("Updating the node ", nn, " with status: ", job_state))

        # Tag job status
        self$update_dag(
          name = nn,
          field_to_update = "status",
          update_value = job_state
        )
      }

      rm(tmplist)
      return(0)
    }
  )
)
