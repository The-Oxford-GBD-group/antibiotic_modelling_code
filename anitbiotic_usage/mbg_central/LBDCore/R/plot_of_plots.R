#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param reg PARAM_DESCRIPTION
#' @param age PARAM_DESCRIPTION
#' @param nfolds PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot_of_plots
#' @export
plot_of_plots <- function(run_date,
                          reg,
                          age,
                          nfolds = 5) {


  ###########
  ## PLOT OF PLOTS FUNCTION FOR TESTING AGGREGATED OOS RESULTS IN STACKING

  # load in neeed data
  folds <- c()
  for (i in 1:nfolds) {
    if (file.exists(sprintf("%s/output/%s/fin_bin%i_%s_%i", sharedir, run_date, age, reg, i))) {
      folds <- c(folds, i)
    }
  }
  message(paste("The Following Folds (of", nfolds, ") are ready and will be used:"))
  message(paste(folds, collapse = ", "))

  if (length(folds) > 0) {
    d <- data.table()
    message("\nLoading data:")
    for (i in folds) {
      message(i)
      patt <- sprintf("%s_aggval_bin%i_%s_%i", indicator, age, reg, i)
      for (f in list.files(path = sprintf("%s/output/%s/", sharedir, run_date), pattern = patt)) {
        message(f)
        tmp <- fread(sprintf("%s/output/%s/%s", sharedir, run_date, f))
        tmp$model <- gsub(".csv", "", gsub(paste0(patt, "_"), "", f))
        d <- rbind(d, tmp)
      }
    }

    # simplify d for now
    d <- d[, c("p", "mean", "error", "model", "ho_id", "year", "fold", "age", "exposure", "clusters_covered_95", "total_clusters"), with = F]

    # coverage
    # d[,cov:=clusters_covered_95/total_clusters]

    # mean error
    me <- aggregate(error ~ model, d, mean)
    names(me) <- c("model", "mean_error")

    # RMSE
    rmse <- aggregate(error ~ model, d, function(x) {
      sqrt(mean(x^2))
    })
    names(rmse) <- c("model", "rmse")

    # correlation
    corr <- plyr::ddply(d, "model", function(x) cor(x$p, x$mean))
    names(corr) <- c("model", "correlation")

    res <- data.frame(model = corr$model)
    for (pv in c("me", "rmse", "corr"))
      res <- merge(res, get(pv), by = "model")



    # plot
    p1 <- ggplot(res, aes(x = correlation, y = rmse, colour = model, shape = model)) + geom_point() +
      theme_bw() +
      theme(legend.position = "none")
    p2 <- ggplot(res, aes(x = correlation, y = mean_error, colour = model, shape = model)) + geom_point() +
      theme_bw() +
      theme(legend.position = "none")
    p3 <- ggplot(res, aes(x = rmse, y = mean_error, colour = model, shape = model)) + geom_point() +
      theme_bw()
    legend <- get_legend(p3)
    p3 <- p3 + theme(legend.position = "none")

    message(paste0("saving ", sprintf("%s/output/%s/plot_of_plots_bin%i_%s.pdf", sharedir, run_date, age, reg)))
    pdf(sprintf("%s/output/%s/plot_of_plots_bin%i_%s.pdf", sharedir, run_date, age, reg))
    grid.arrange(p1, p2, p3, legend, ncol = 2, top = sprintf("OOS Predictive Validity: Age Bin %i, %s. %i/%i folds analyzed", age, reg, length(folds), nfolds))
    dev.off()
  } else {
    message("NO DATA WRITTEN YET, SO NO PLOTS")
  }
  # return(plot)
}
