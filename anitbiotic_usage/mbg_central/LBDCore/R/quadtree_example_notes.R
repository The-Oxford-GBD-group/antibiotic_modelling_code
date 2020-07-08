#' @title Quadtree Example Notes
#' @return Notes
#' @export
quadtree_example_notes <- function() {
  cat('
      # ## quadtree example \n
      # n <- 25000         # Points per cluster \n
      # n_centers <- 40  # Number of cluster centers \n
      # sd <- 1/2        # Standard deviation of each cluster \n
      # set.seed(17) \n
      # centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE) \n
      # xy <- matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE) \n
      # k <- 5 \n
      # system.time(qt <- quadtree(xy, k)) \n
      # \n
      # xylim <- cbind(x=c(min(xy[,1]), max(xy[,1])), y=c(min(xy[,2]), max(xy[,2]))) \n
      # plot(xylim, type="n", xlab="x", ylab="y", main="Quadtree") \n
      # lines(qt, xylim, col="Gray") \n
      # points(qt, pch=16, cex=0.5) \n
      # \n
      # ## quadtree_ct example \n
      # ## test it out on some simulated data with different sample sizes at each point \n
      # n <- 20          # Points per cluster \n
      # n_centers <- 10  # Number of cluster centers \n
      # sd <- 1/2        # Standard deviation of each cluster \n
      # set.seed(17) \n
      # centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE) \n
      # xy <- matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE) \n
      # ss <- c(rep(1, n*n_centers/2), rep(5, n*n_centers/2)) \n
      # n <- 1           # Points per cluster \n
      # n_centers <- 5   # Number of cluster centers \n
      # sd <- 1/2        # Standard deviation of each cluster \n
      # centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE) \n
      # xy <- rbind(xy, \n
      # matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE)) \n
      # ss <- c(ss, rep(20, n*n_centers)) \n
      # k <- 5 \n
      # system.time(qt <- quadtree_ct(xy, ss, 20, min_in_bin=1)) \n
      # \n
      # ## plot \n
      # xylim <- cbind(x=c(min(xy[,1]), max(xy[,1])), y=c(min(xy[,2]), max(xy[,2]))) \n
      # png("qt_test.png") \n
      # plot(xylim, type="n", xlab="x", ylab="y", main="Quadtree w/ max SS = 20") \n
      # lines(qt, xylim, col="Gray") \n
      # points(qt, pch=16, cex=0.5, alpha=0.9) \n
      # points(xy, col=as.factor(ss), pch=16) \n
      # legend("bottomright", legend=c("1", "5", "20"), col=1:3, pch=16) \n
      # dev.off() \n
      
      ')
}
