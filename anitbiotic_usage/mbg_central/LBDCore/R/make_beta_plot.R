#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname make_beta_plot
#' @export
make_beta_plot <- function(indicator_group,
                           indicator,
                           run_date,
                           region) {

  # fit_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/fit_stats')
  # load(paste0(fit_dir,'/strata.RData'))
  # Regions=strata
  model_data <- lapply(region, process_betas)
  model_data <- do.call(rbind.fill, model_data)
  model_data <- as.data.table(model_data)
  model_data <- model_data[!grepl("gaul_", cov), ] # Remove country fixed effects from this plot
  selected_gg <- ggplot(model_data, aes(x = cov, y = mean)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.25) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 15)) +
    ggtitle("Selected model") +
    theme(strip.text.x = element_text(size = 15))

  return(selected_gg)
}
