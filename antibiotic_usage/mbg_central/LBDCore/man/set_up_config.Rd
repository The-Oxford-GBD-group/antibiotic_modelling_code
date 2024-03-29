% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_config.R, R/load_config.R,
%   R/set_up_config.R
\name{check_config}
\alias{check_config}
\alias{load_config}
\alias{set_up_config}
\title{FUNCTION_TITLE}
\usage{
check_config(cr = core_repo)

load_config(...)

set_up_config(repo, core_repo = repo, indicator_group, indicator,
  config_name = NULL, config_file = NULL, covs_name = NULL,
  covs_file = NULL, post_est_only = FALSE, run_date = "",
  push_to_global_env = TRUE, run_tests = TRUE)
}
\arguments{
\item{cr}{PARAM_DESCRIPTION, Default: core_repo}

\item{repo}{Location where you've cloned "mbg" repository.}

\item{core_repo}{Location where you've cloned the lbd_core repository. Not necessary in the package version.}

\item{indicator_group}{Category of indicator, i.e. "education"}

\item{indicator}{Specific outcome to be modeled within indicator category, i.e. "edu_0"}

\item{config_name}{PARAM_DESCRIPTION, Default: NULL}

\item{config_file}{Full path to configuration file that overrides \code{config_name}, Default: NULL}

\item{covs_name}{PARAM_DESCRIPTION, Default: NULL}

\item{covs_file}{Full path to covariates configuration file that overrides \code{covs_name}, Default: NULL}

\item{post_est_only}{PARAM_DESCRIPTION, Default: FALSE}

\item{run_date}{PARAM_DESCRIPTION, Default: ''}

\item{push_to_global_env}{Should the config parameters be pushed to the global environment? Default: TRUE}

\item{run_tests}{Run the assertion tests? This will run the tests and error out if there's an
inconsistent config parameter. Default: TRUE}

\item{repo}{Location where you've cloned the MBG repository for your indicator.}

\item{indicator_group}{Category of indicator, i.e. "education"}

\item{indicator}{Specific outcome to be modeled within indicator category, i.e. "edu_0"}

\item{config_name}{Name of configuration file in the indicator folder, Default: NULL}

\item{covs_name}{Name of covariates configuration file, Default: NULL}

\item{post_est_only}{Set up only for post estimation? Default: FALSE}

\item{run_date}{Run date, Default: ''}
}
\value{
OUTPUT_DESCRIPTION

OUTPUT_DESCRIPTION

A named list of configs, where "config" is the usual MBG config, and "fixed_effects_config" is the
config info of the fixed effects
}
\description{
FUNCTION_DESCRIPTION

FUNCTION_DESCRIPTION

Setting up configuration variables for an MBG run
}
\details{
DETAILS

DETAILS

DETAILS
}
\note{
Need to use an object instead of pushing to global scope

THIS FUNCTION HAS BEEN DEPRECATED IN FAVOR OF SET_UP_CONFIG
}
\examples{
\dontrun{
if (interactive()) {
  # EXAMPLE1
}
}
\dontrun{
if (interactive()) {
  config <- load_config(
    repo = core_repo,
    indicator_group = indicator_group,
    indicator = indicator,
    config_name = "config_training",
    covs_name = "covs_training"
  )
}
}
}
