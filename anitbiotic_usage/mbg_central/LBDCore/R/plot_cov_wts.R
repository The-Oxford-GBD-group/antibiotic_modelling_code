## #########################
## plot the output from cov wts in a heatmap in ggplot
## input is same as get.cov.wts() function)
## output is a ggplot object
## #########################
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rd PARAM_DESCRIPTION
#' @param ind PARAM_DESCRIPTION
#' @param ind_gp PARAM_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param plot.inla.col PARAM_DESCRIPTION, Default: TRUE
#' @param age PARAM_DESCRIPTION, Default: 0
#' @param holdout PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}}
#' @rdname plot.cov.wts
#' @export
#'
plot.cov.wts <- function(rd, ## run_date
                         ind, ## indicator
                         ind_gp, ## indicator_group
                         reg,
                         plot.inla.col = TRUE,
                         age = 0,
                         holdout = 0) {

  ## load a cov.wts object
  load(paste0(
    "/share/geospatial/mbg/", ind_gp, "/", ind, "/output/", rd,
    sprintf("/cov_wts_%s_holdout_%i.RData", reg, holdout)
  ))
  cov.wts <- imp.mat

  ## remove the final column which has inla weight
  if (!plot.inla.col) {
    cov.wts <- cov.wts[, -ncol(cov.wts)]
  } else {
    ## rescale to be -1:1
    cov.wts[, ncol(cov.wts)] <- cov.wts[, ncol(cov.wts)] / sum(cov.wts[, ncol(cov.wts)], na.rm = TRUE)
  }

  ## melt
  cw.m <- as.data.table(reshape2::melt(as.matrix(cov.wts)))
  colnames(cw.m)[1:3] <- c("Model", "Covar", "Imp")
  ## cw.m <- na.omit(cw.m)

  ## reorder factors
  cw.m$Model <- factor(cw.m$Model, levels = c("INLA COMBINED", sort(setdiff(unique(cw.m$Model), "INLA COMBINED"))))

  ## setup the plot

  ## pdf("~/test2.pdf", width = 10, height = 3)
  base_size <- 9
  p <- ggplot(cw.m, aes(Covar, Model)) +
    geom_tile(aes(fill = Imp), colour = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
    theme_grey(base_size = base_size) +
    labs(x = "Covariate", y = "Model") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(axis.text.x = element_text(
      size = base_size * 0.8,
      angle = 270,
      hjust = 0,
      colour = "grey50"
    ))
  ## print(p)
  ## dev.off()
  return(p)
}
