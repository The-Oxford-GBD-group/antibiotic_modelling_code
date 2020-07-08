#' @title Helper object for dealing with covariate paths
#' @description CovariatePathHelper turns covariate/measure/release data into paths via \code{covariate_paths()}
#'  and converts it back via \code{covariate_metadata_from_path}. This is used internally by
#'  \code{MbgStandardCovariateLoader}.
#' @examples
#' \dontrun{
#'
#' covariates <- c("access", "evi")
#' measures <- c("mean", "median")
#' releases <- c("2019_06_10", "2019_06_10")
#' helper <- CovariatePathHelper$new()
#' paths <- helper$covariate_paths(covariates = covariates,
#'                                 measures = measures,
#'                                 releases = releases)
#' metadata <- helper$covariate_metadata_from_path(paths[1])
#' }
#' @rdname CovariatePathHelper
#' @export
CovariatePathHelper <- R6::R6Class("CovariatePathHelper",
  public = list(
    cov_dir = "/home/j/WORK/11_geospatial/01_covariates/00_MBG_STANDARD",

    covariate_paths = function(covariates, measures = NULL, releases = NULL) {
      if (is.null(measures)) {
        file.path(self$cov_dir, covariates)
      } else if (is.null(releases)) {
        file.path(self$cov_dir, covariates, measures)
      } else {
        file.path(self$cov_dir, covariates, measures, releases)
      }
    },
    covariate_metadata_from_path = function(path) {
      # remove cov_dir from string (will now begin with "/") and split on "/" (accounting for OS platform)
      pieces <- strsplit(sub(self$cov_dir, "", path), .Platform$file.sep, fixed = TRUE)[[1]]
      # first value is "", subsequent values are interesting
      result <- list(
        covariate = pieces[2],
        measure = pieces[3],
        release = pieces[4]
      )
      return(result)
    },
    newest_covariate_release = function(covariate_paths) {
      # USE.NAMES = FALSE causes the returned vector to only support numeric indexing
      return(sapply(covariate_paths, private$newest_covariate_release_single, USE.NAMES = FALSE))
    }
  ), # end public
  private = list(
    newest_covariate_release_single = function(path) {
      all.dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
      release.dirs <- all.dirs[grep("^\\d{4}_\\d{2}_\\d{2}$", all.dirs)]
      return(max(release.dirs))
    }
  ) # end private
)
