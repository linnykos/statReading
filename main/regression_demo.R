rm(list=ls())
density_func <- function(x,y){
  mvtnorm::dmvnorm(c(x,y), mean = c(-1,1), sigma = matrix(c(2,-1,-1,2),2,2))
}

xlim <- c(-5, 5); ylim <- c(-5, 5)
mat <- create_prob_grid(xlim, ylim, density_func)
plot_grid(mat, asp = T)

dat <- sample_from_grid(mat)
graphics::points(dat, pch = 16)
res <- ols(dat)

plot_ols(res, xlim, lwd = 2)
