#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mbg_fixed_effects PARAM_DESCRIPTION
#' @param simple_polygon PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_mbg_covariates
#' @export
load_mbg_covariates <- function(mbg_fixed_effects, simple_polygon) {

  # Hard-code MBG covariates that have best models available
  mbg_covs <- c("edu_0")

  # Load MBG covariates available
  edu_0 <- brick("/share/geospatial/mbg/education/edu_0/output/best/edu_0.tif")
  edu_mean <- brick("/share/geospatial/mbg/education/edu_mean/output/best/edu_mean.tif")

  # Construct list of covariates to GAM and use in model from fixed_effects parameter equation.
  selected_covs <- strsplit(mbg_fixed_effects, " ")
  selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]
  num_covs <- length(selected_covs)
  lcovs <- list()
  for (i in 1:num_covs) {
    this_cov <- selected_covs[i]
    lcovs[[i]] <- get(this_cov)
    names(lcovs[[1]]) <- gsub("period_", paste0(this_cov, "."), names(lcovs[[1]]))
    names(lcovs)[i] <- this_cov
  }

  # Make sure covariate layers line up with raster we are modeling over
  for (l in 1:length(lcovs)) {
    lcovs[[l]] <- extend(lcovs[[l]], extent(-180, 180, -90, 90), keepres = TRUE)
    lcovs[[l]] <- crop(lcovs[[l]], extent(simple_polygon))
    lcovs[[l]] <- setExtent(lcovs[[l]], simple_polygon)
    lcovs[[l]] <- mask(lcovs[[l]], simple_polygon)
  }

  return(lcovs)
}
