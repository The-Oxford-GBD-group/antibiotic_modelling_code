#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pull_year PARAM_DESCRIPTION
#' @param year_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_admin_preds
#' @export
get_admin_preds <- function(pull_year, year_list) {
  proj <- ifelse(pull_year %in% year_list, F, T)

  if (proj == T) {

    # Load proj_draws object if working with projected data
    message("-- Loading proj_draws object...")
    proj_draws <- readRDS(sprintf(
      "%s/%s_%s_%s_projections_adm%i_draw_matrix%s.RDs",
      proj_dir, ind, measure, pull_year, aa,
      ifelse(uselogit, "_logit", "")
    ))
  } else if (proj == F) {

    # Load admin preds object if working with non-projected data
    # That is, if year is within the modeling frame

    message("-- Loading admin pred object")
    ## load the admin objects
    ## try two different locations until we standardize
    file_1 <- sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/%s_%s_admin_draws_raked.Rdata",
      ind_gp, ind, rd, ind, measure
    )
    file_2 <- paste0(
      "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd, "/",
      ind, "_raked_admin_draws_eb_bin0_0.RData"
    )

    if (file.exists(file_1)) {
      load(file_1)
    } else if (file.exists(file_2)) {
      load(file_2)
    } else {
      stop("Cannot load admin pred object!")
    }

    # Create a proj_draws object (same name for convenience)
    proj_draws <- get(paste0("admin_", aa))
    proj_draws <- subset(proj_draws, year == pull_year)

    # Format to be the same as the other proj_draws object
    proj_draws <- subset(proj_draws, select = names(proj_draws)[!(names(proj_draws) %in% c("year", "pop"))])
    setnames(proj_draws, paste0("ADM", aa, "_CODE"), "spatial_idx")
  }

  return(as.data.table(proj_draws))
}
