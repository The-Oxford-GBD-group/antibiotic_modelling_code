% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_is_oos_draws.R
\name{get_is_oos_draws}
\alias{get_is_oos_draws}
\title{FUNCTION_TITLE}
\usage{
get_is_oos_draws(ind_gp, ind, rd, ind_fm = "binomial", age = 0,
  nperiod = 16, yrs = 2000:2015, write.to.file = FALSE,
  get.oos = FALSE, year_col = "original_year",
  shapefile_version = "current", ...)
}
\arguments{
\item{ind_gp}{PARAM_DESCRIPTION}

\item{ind}{PARAM_DESCRIPTION}

\item{rd}{PARAM_DESCRIPTION}

\item{ind_fm}{PARAM_DESCRIPTION, Default: 'binomial'}

\item{age}{PARAM_DESCRIPTION, Default: 0}

\item{nperiod}{PARAM_DESCRIPTION, Default: 16}

\item{yrs}{PARAM_DESCRIPTION, Default: 2000:2015}

\item{write.to.file}{PARAM_DESCRIPTION, Default: FALSE}

\item{get.oos}{PARAM_DESCRIPTION, Default: FALSE}

\item{year_col}{PARAM_DESCRIPTION, Default: 'original_year'}

\item{shapefile_version}{PARAM_DESCRIPTION, Default: 'current'}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if (interactive()) {
  # EXAMPLE1
}
}
}
\seealso{
\code{\link[rgdal]{readOGR}}
 \code{\link[sp]{over-methods}},\code{\link[sp]{over}}
 \code{\link[raster]{extract}}
}
