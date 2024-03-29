% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_admin_years.R
\name{compare_admin_years}
\alias{compare_admin_years}
\title{Compare admin years}
\usage{
compare_admin_years(ind_gp, ind, rd, measure, start_year, end_year,
  admin_level, uselogit, year_list, summstats = c("mean", "upper",
  "lower", "cirange"), shapefile_version = "current")
}
\arguments{
\item{ind_gp}{indicator group}

\item{ind}{indicator}

\item{rd}{run_date}

\item{measure}{prevalence, incidence, mortality, etc}

\item{start_year}{baseline year for analysis}

\item{end_year}{comparison year for analysis}

\item{admin_level}{What admin level should the target be evaluated at?}

\item{uselogit}{Were projections created in logit space?}

\item{shapefile_version}{character string indicating version of shapefile to pull}

\item{proj}{Is \code{target_year} projected (\code{proj = T}) or modeled (\code{proj = F})?}
}
\value{
A data table by admin unit with summary stats summarized for the difference
}
\description{
Compare two admin years: what was the difference between them (with CIs)?
}
\examples{
\dontrun{
difference_df <- compare_admin_years(
  ind_gp = indicator_group,
  ind = indicator,
  rd = run_date,
  measure = "prevalence",
  start_year = 2000,
  end_year = 2015,
  admin_level = 2,
  uselogit = T,
  year_list = year_list,
  summstats = c("mean", "upper", "lower", "cirange")
)
}
}
