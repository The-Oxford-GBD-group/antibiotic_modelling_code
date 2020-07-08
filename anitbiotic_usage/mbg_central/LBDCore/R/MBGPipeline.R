#' @title MBG Pipeline Class
#' @docType class
#' @keywords pipeline MBG
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} with the pipeline.
#' @format \code{\link{R6Class}} object.
#'
#'
#' @field indicator_group Indicator group, for e.g. "training"
#' @field indicator Indicator name, for e.g. "tr_had_diarrhea"
#' @field config_name Name of config file (not recommended)
#' @field config_file Path to config file CSV (recommended using this)
#' @field covs_name Name of covariates config file (not recommended)
#' @field covs_file Path to covariates config file CSV (recommended using this)
#' @field run_date Initialize using a pre-made run-date if desired. The method \code{setup_rundate} can be used to tinker with run dates after this.
#' @field core_repo Path to \code{lbd_core} repository.
#'
#' @section Methods:
#' \describe{
#' \item{\code{print}}{
#'   Prints the pipeline info
#' }
#'
#' \item{\code{parse_conf}}{
#'  Parse a config parameter
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{conf_key}: Config parameter
#'     }
#'     \item{\code{conf_dt}: Config data.table in the Pipeline class.
#'     }
#'   }
#' }
#' \item{\code{eval_conf}}{
#'  Evaluate a config parameter
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{conf_key}: Config parameter
#'     }
#'     \item{\code{conf_dt}: Config data.table in the Pipeline class.
#'     }
#'   }
#' }
#'
#' \item{\code{setup_conf}}{
#'  Wraps \code{set_up_config} to set up the config files
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{push_to_global_env}: Should the config parameters be pushed
#'     to the global environment? Highly recommend against this.
#'     }
#'     \item{\code{run_tests}: Run all the assertion tests for config params.
#'     }
#'   }
#' }
#'
#' \item{\code{setup_rundate}}{
#'  Create or pass through a run date, with the option to do a full clean-up
#'  before creating the directory structure
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{run_date}: Create a new run date, or use the one initialized
#'     in the pipeline if set to \code{NULL}. If this argument and the
#'     initialized run date are left to be \code{NULL}, then a new time stamp is
#'     created.
#'     }
#'     \item{\code{full_cleanup}: If set to \code{TRUE}, then the run date
#'     folder will be cleaned up and then remade. Default: \code{FALSE}
#'     }
#'     \item{\code{suffix}: Suffix to add to run date? Default: \code{""}
#'     }
#'     \item{\code{prefix}: Prefix to add to run date? Default: \code{""}
#'     }
#'   }
#' }
#'
#' \item{\code{make_holdouts}}{
#'  Makes holdouts, but depends on whether the config argument
#'  \code{makeholdouts} is set to \code{TRUE} or not.
#'   \emph{Arguments:}
#'   \itemize{
#'     \item{\code{withdate}: Used in \code{load_input_data}
#'     }
#'     \item{\code{date}: Run date
#'     }
#'   }
#' }
#'
#' \item{\code{create_loopvars}}{
#'  Create table of region, age and holdouts to loop over, aka the
#'  loopvars table (TM).
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#' Pipeline_Class <- MBGPipeline$new(
#'   core_repo = "/share/code/geospatial/sadatnfs/lbd_core/",
#'   indicator_group = "training",
#'   indicator = "tr_had_diarrhea",
#'   config_file = "/ihme/code/geospatial/sadatnfs/lbd_core/training/config_training_pipeline.csv",
#'   covs_name = "/ihme/code/geospatial/sadatnfs/lbd_core/training/covs_training.csv"
#' )
#' Pipeline_Class$setup_conf(
#'   push_to_global_env = FALSE,
#'   run_tests = TRUE
#' )
#' Pipeline_Class$setup_rundate(run_date = "run_MBG_run")
#' Pipeline_Class$make_holdouts()
#' Pipeline_Class$create_loopvars()
#' }
#'
MBGPipeline <- R6::R6Class("MBGPipeline",

  # lock_objects = FALSE is necessary for being able to add more
  # to self after initializing
  lock_objects = FALSE,
  public = list(
    core_repo = NULL,
    indicator_group = NULL,
    indicator = NULL,
    config_name = NULL,
    config_file = NULL,
    covs_name = NULL,
    covs_file = NULL,
    run_date = NULL,

    # __init__
    initialize = function(indicator_group,
                              indicator,
                              config_name = NULL,
                              config_file = NULL,
                              covs_name = NULL,
                              covs_file = NULL,
                              run_date = NULL,
                              core_repo) {
      stopifnot(!is.null(core_repo))
      stopifnot(!is.null(indicator_group))
      stopifnot(!is.null(indicator))

      # Stop if both _name and _file for configs are NULL
      stopifnot(!is.null(config_name) | !is.null(config_file))
      stopifnot(!is.null(covs_name) | !is.null(covs_file))

      # Add to self
      self$core_repo <- core_repo
      self$indicator_group <- indicator_group
      self$indicator <- indicator

      self$config_name <- config_name
      self$config_file <- config_file
      self$covs_name <- covs_name
      self$covs_file <- covs_file
      self$user <- Sys.info()["user"]
      self$run_date <- run_date

      self$sharedir <- paste("/share/geospatial/mbg",
        self$indicator_group, self$indicator,
        sep = "/"
      )
      self$commondir <- paste(self$core_repo,
        "mbg_central/share_scripts/common_inputs",
        sep = "/"
      )

      self$print()
    },

    # Printing the class
    print = function(...) {
      cat("MBG Pipeline: \n")
      cat("  Repo Path        :  ", self$core_repo, "\n", sep = "")
      cat("  Indicator Group  :  ", self$indicator_group, "\n", sep = "")
      cat("  Indicator        :  ", self$indicator, "\n", sep = "")
      if (is.null(self$config_name)) {
        cat("  Model Path       :  ", self$config_file, "\n", sep = "")
      } else {
        cat("  Model Name       :  ", self$config_name, "\n", sep = "")
      }

      if (is.null(self$covs_name)) {
        cat("  Covs Path        :  ", self$covs_file, "\n", sep = "")
      } else {
        cat("  Covs Config      :  ", self$covs_name, "\n", sep = "")
      }
    },



    # Helper function: parsing values from a typical config data.table
    parse_conf = function(conf_key, conf_dt = self$config) {
      return(conf_dt[V1 == paste0(conf_key), V2])
    },

    # Helper function to eval and parse a config key
    eval_conf = function(conf_key, conf_dt = self$config) {
      return(eval(parse(text = self$parse_conf(conf_key, conf_dt))))
    },

    # Setting up configs
    setup_conf = function(push_to_global_env = FALSE, run_tests = TRUE) {

      # Set up config first
      print("Config Setup: [1/3] Getting config info and running checks")
      config_both <- set_up_config(
        repo = self$core_repo,
        core_repo = self$core_repo,
        indicator_group = self$indicator_group,
        indicator = self$indicator,
        config_name = self$config_name,
        config_file = self$config_file,
        covs_name = self$covs_name,
        covs_file = self$covs_file,
        push_to_global_env = push_to_global_env,
        run_tests = run_tests
      )

      # Separate out regular and FE configs
      self$config <- config_both[["config"]]
      self$fixed_effects_config <- config_both[["fixed_effects_config"]]

      # Create a few objects from the config file loaded above
      print(paste0(
        "Config Setup: [2/3] Checks for values of Regions,",
        " year_list and summstats"
      ))

      # Push the above fixes to global env if supplied
      if (push_to_global_env) {
        if (class(self$eval_conf("Regions")) == "character" &
          length(self$eval_conf("Regions")) == 1) {
          assign("Regions", parse_conf("Regions"), envir = .GlobalEnv)
        }

        if (class(self$eval_conf("year_list")) == "character") {
          assign("year_list", parse_conf("year_list"), envir = .GlobalEnv)
        }

        if (length(self$eval_conf("summstats")) == 1 &
          grepl(",", self$parse_conf("summstats"))) {
          assign("summstats", parse_conf("summstats"), envir = .GlobalEnv)
        }
      }


      # Load GADM list
      print("Config Setup: [3/3] Loading list of GADM codes")
      self$gadm_list <- get_adm0_codes(self$eval_conf("Regions"),
        shapefile_version = self$parse_conf("modeling_shapefile_version")
      )

      # If running individual countries, get set up
      if (self$parse_conf("individual_countries")) {

        # Convert all Regions to individual countries
        Regions <- get_individual_countries(self$gadm_list)

        # Turn off all FEs in config and gloabl env
        self$config[V1 == "use_child_country_fes", V2 := FALSE]
        self$config[V1 == "use_inla_country_fes", V2 := FALSE]
        self$config[V1 == "use_inla_country_res", V2 := FALSE]

        # Push the above fixes to global env if supplied
        if (push_to_global_env) {
          assign("use_child_country_fes", FALSE, envir = .GlobalEnv)
          assign("use_inla_country_fes", FALSE, envir = .GlobalEnv)
          assign("use_inla_country_res", FALSE, envir = .GlobalEnv)
        }
      }

      # Finally, let's create a named list out of the config into 'config_list'
      self$config_list <- as.list(self$config[, V2])
      names(self$config_list) <- self$config[, V1]

      # SPECIAL: Parse year_list and z_list because it's used as the vector and
      # we want to make sure there's no ad-hoc conflict in types
      self$config_list$year_list <- eval(
        parse(
          text = self$config_list$year_list
        )
      )
      self$config_list$z_list <- eval(
        parse(
          text = self$config_list$z_list
        )
      )
    },

    # Setting up rundate
    setup_rundate = function(run_date = NULL,
                                 full_cleanup = FALSE,
                                 suffix = "",
                                 prefix = "") {
      if (full_cleanup) {
        unlink(
          file.path(
            "/share/geospatial/mbg",
            self$indicator_group,
            self$indicator,
            "output",
            self$run_date
          ),
          recursive = TRUE
        )
      }

      if (is.null(self$run_date) & is.null(run_date)) {
        print("Setting up run_date...")
        self$run_date <- paste0(make_time_stamp(TRUE))
        print(self$run_date)
      } else if (!is.null(self$run_date) & is.null(run_date)) {
        run_date <- self$run_date
        print(paste0(
          "run_date supplied as value in initializing pipeline: ",
          self$run_date
        ))
      } else if (is.null(self$run_date) & !is.null(run_date)) {
        self$run_date <- run_date
        print(paste0("run_date supplied as value in this function: ", run_date))
      } else {
        warning(paste0(
          "Run-date supplied from both initializing pipeline",
          " and from this function. Using only the newly",
          " supplied run_date..."
        ))
        self$run_date <- run_date
      }

      # Adding suffix/prefix to self$run_date
      self$run_date <- paste0(prefix, self$run_date, suffix)

      # Set up outputdir as sge logs folders based off of run_date
      self$outputdir <- file.path(
        "/share/geospatial/mbg",
        self$indicator_group,
        self$indicator,
        "output",
        self$run_date
      )

      # Create the output and error log folders
      dir.create(self$outputdir)
      dir.create(
        file.path(
          self$outputdir,
          "errors"
        )
      )
      dir.create(
        file.path(
          self$outputdir,
          "output"
        )
      )
    },



    # Make holdouts
    make_holdouts = function(withdate = FALSE,
                                 date = self$run_date) {


      # NOTE: make_holdouts() will not run if set to false in config
      if (self$eval_conf("makeholdouts")) {

        # Load the full input data
        self$df <- load_input_data(
          indicator = self$indicator,
          simple = NULL,
          removeyemen = TRUE,
          years = self$parse_conf("yearload"),
          yl = self$parse_conf("year_list"),
          withtag = self$eval_conf("withtag"),
          datatag = self$parse_conf("datatag"),
          use_share = self$parse_conf("use_share"),
          withdate = withdate,
          date = self$run_date
        )

        # Add in location information
        self$df <- merge_with_ihme_loc(self$df,
          shapefile_version = self$parse_conf("modeling_shapefile_version")
        )

        # Make a list of dfs for each region,
        # with 5 qt folds identified in each
        self$stratum_ho <- make_folds(
          data = self$df,
          n_folds = self$parse_conf("n_ho_folds"),
          spat_strat = "qt",
          temp_strat = "prop",
          strat_cols = "region",
          ts = self$parse_conf("ho_ts"),
          mb = self$parse_conf("ho_mb")
        )
      } else {
        print("makeholdouts in config is set to FALSE. No holdouts created")
      }
    },

    # Create loopvars
    create_loopvars = function() {
      # Make loopvars aka strata grid (format = regions, ages, holdouts)
      if (self$eval_conf("makeholdouts")) {
        self$loopvars <- expand.grid(
          self$eval_conf("Regions"),
          0, 0:self$parse_conf("n_ho_folds")
        )
      } else {
        self$loopvars <- expand.grid(self$eval_conf("Regions"), 0, 0)
      }

      # Clean up names
      self$loopvars <- data.table(self$loopvars)
      colnames(self$loopvars) <- c("region", "age", "holdout")
    },


    # Save the pre-run images
    save_prerun_image = function(ig = self$indicator_group,
                                     indic = self$indicator,
                                     rd = self$run_date) {
      for (i in c(1:nrow(self$loopvars))) {
        img_path <- pre_run_image_path(
          indicator_group = ig,
          indicator = indic,
          run_date = rd,
          age = self$loopvars[i]$age,
          region = self$loopvars[i]$region,
          holdout = self$loopvars[i]$holdout,
          prefix = "DAG"
        )
        save.image(img_path)
        print(paste0("Image saved to ", img_path))
      }
    }
  ) ## End of class ##
)
