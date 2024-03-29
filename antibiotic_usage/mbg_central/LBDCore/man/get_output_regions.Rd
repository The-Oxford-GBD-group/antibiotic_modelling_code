% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_output_regions.R
\name{get_output_regions}
\alias{get_output_regions}
\title{Get output regions}
\usage{
get_output_regions(in_dir)
}
\arguments{
\item{in_dir}{directory path containing completed mbg cell_pred objects}
}
\value{
A vector string of region names
}
\description{
This function takes in a directory where mbg modeling has stored
outputs (*cell_pred* objects) and infers the regions specirfied in
the model

Determines modeling regions from written output dir objects
}
