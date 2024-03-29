% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setompnested.R
\name{setompnested}
\alias{setompnested}
\title{Enables OpenMP nested parallelism}
\usage{
setompnested(enable = TRUE)
}
\arguments{
\item{enable}{A single logical indicating whether or not to enable OpenMP
nested parallelism [default = TRUE]}
}
\value{
None
}
\description{
\code{setompnested()} Uses a function in the "setthreads.so" shared library
built into LBD Singularity images to enable/disable OpenMP nested
parallelism.
}
\details{
Uses a function in the "setthreads.so" shared library built into LBD
Singularity images to enable/disable OpenMP nested parallelism. Since many of
the packages we use in R rely on both OpenMP and MKL, where an OpenMP
parallel region can call the MKL and threading is enabled on both, we want to
enable OMP nested parallelism, which is done by default here (!= 0 is a TRUE,
i.e. requests enabling OMP nested parallelism, see:
https://msdn.microsoft.com/en-us/library/sk3zt8e1.aspx)
The shared library should exist in the LBD Singularity image and should have
already been loaded by \code{mbg_setup()}. This function checks to make sure
that it is loaded and if not, attempts to use \code{load_setthreads()} to do
so. A warning is generated if this is run outside of an LBD Singularity image
and no OMP nesting is done. This function is normally used along with
\code{setmkldynamic()} as follows:
\code{setmkldynamic(enable = FALSE)} and \code{setompnested(enable = TRUE)}
as described here: https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
}
\examples{
\dontrun{
setompnested(enable = TRUE) # disables OMP nesting
}

}
\seealso{
This function depends on:
\code{\link{load_setthreads()}}
And is related to:
\code{\link{setmkldynamic()}}
\code{\link{setmklthreads()}}
\code{\link{setompthreads()}}

Other Mutlti-threading Functions: \code{\link{get_max_forked_threads}},
  \code{\link{get_mkl_threads}},
  \code{\link{get_omp_threads}},
  \code{\link{get_total_threads}},
  \code{\link{setmkldynamic}}, \code{\link{setmklthreads}},
  \code{\link{setompthreads}}
}
\author{
Ian M. Davis, \email{imdavis@uw.edu}
}
\concept{Mutlti-threading Functions}
