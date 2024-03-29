% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_git_status.R
\name{record_git_status}
\alias{record_git_status}
\title{Record Git Status}
\usage{
record_git_status(core_repo, indic_repo = NULL, show_diff = FALSE,
  check_core_repo = TRUE, file = NULL)
}
\arguments{
\item{core_repo}{file path to the lbd_core repo}

\item{indic_repo}{file path to an indicator-specific repo (optional)}

\item{show_diff}{logical. If there are uncomitted changes, should the
output from git diff be shown?}

\item{check_core_repo}{logical. Will check whatever has been set as
'core_repo' is the default LBD core code master repo and will give
messages and warnings if not.}

\item{file}{file path to a text file where output should be saved. This
is optional. If no file path is provided, the output will instead be
printed to the screen.}
}
\value{
Git status
}
\description{
Retrieve information about current git status (eg, branch, commit,
uncommitted changes) for core repository and optionally a separate
indicator repository. A check can also be made to see if your core
repository is in sync with the LBD core repo
(\code{/share/code/geospatial/lbd_core}) if a fork is being used.

Can be used at a minimum to print the git hash of the code being used
for posterity.
}
