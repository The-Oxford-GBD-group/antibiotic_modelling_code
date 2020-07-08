#' @title Creates plots of component smooth objects from fitted gam model
#'
#' @description This function creates plots in a standardized 4x3 format, saved to
#' standardized directories and filenames
#'
#' @param child_model_list Nested list of child model objects (models within regions)
#'                         such that \code{child_model_list[["cssa"]][["gam"]]} returns the
#'                         appropriate gam model object for region cssa
#' @param regions character vector of regions
#' @param o_dir output directory
#' @return writes png files in standardized format to `o_dir\\gam\\[region]\\`
#' @examples
#' \dontrun{
#' message("Loading child models for each region")
#' child_model_list <- lapply(Regions, function(reg) {
#'   child_model_file <- paste0(sharedir, "child_model_list_", reg, "_0.RData")
#'   load(child_model_file, verbose = F)
#'   return(child_models)
#' })
#' 
#' names(child_model_list) <- Regions
#' 
#' # Plot GAM models
#' plot_gam_models(
#'   child_model_list = child_model_list,
#'   regions = Regions,
#'   o_dir = out_dir
#' )
#' }
#' 
#' @export
plot_gam_models <- function(child_model_list, regions, o_dir) {

  # Create gam plots in a standardized output format (3x3 grid) for each
  # of the child models (for each region)

  str_match <- stringr::str_match

  for (reg in regions) {

    # Set up directory
    dirname <- paste0(o_dir, "gam/", reg, "/")
    dir.create(dirname, recursive = T, showWarnings = F)

    # Load child model
    gam_child <- child_model_list[[reg]][["gam"]]
    labs <- sapply(gam_child$smooth, function(x) return(x$label))
    n_plots <- length(labs)

    # Create list of which plots go in which pages
    x <- 1:n_plots
    length(x) <- suppressWarnings(prod(dim(matrix(x, ncol = 12, byrow = T))))
    x <- matrix(x, ncol = 12, byrow = T)

    for (i in 1:nrow(x)) {

      # Set up each page (row in layout matrix) & plot

      terms <- x[i, ]
      terms <- terms[!is.na(terms)]

      filename <- paste0(dirname, "gam_plot_", reg, "_page_", i, ".png")

      png(
        filename = filename,
        type = "cairo",
        units = "in",
        width = 12,
        height = 8,
        pointsize = 14,
        res = 200
      )

      par(mfrow = c(3, 4))
      for (z in terms) {
        plot(gam_child, select = z)
      }

      dev.off()
    }
  }
}
