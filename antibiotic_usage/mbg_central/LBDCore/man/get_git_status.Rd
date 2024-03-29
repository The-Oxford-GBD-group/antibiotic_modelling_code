% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_git_status.R
\name{get_git_status}
\alias{get_git_status}
\title{Get Git Status}
\usage{
get_git_status(repo, repo_name, show_diff = FALSE)
}
\arguments{
\item{repo}{The git repository directory}

\item{repo_name}{The name of the repo to print in the header}

\item{show_diff}{Print the diff}
}
\value{
character vector of a report intended to be printed with cat
}
\description{
Given a path to a directory, determine if it is a git repository, and if so,
return a string of git hash, branch, date of last commit, and status plus the
diff if requested
}
