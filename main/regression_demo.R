rm(list=ls())
set.seed(10)

density_func <- function(x,y){
  val1 <- mvtnorm::dmvnorm(c(x,y), mean = c(-2,2), sigma = 2*matrix(c(2,-1,-1,2),2,2))
  val2 <- mvtnorm::dmvnorm(c(x,y), mean = c(-1.8,1.8), sigma = 2*matrix(c(2,1,1,2),2,2))
  val3 <- mvtnorm::dmvnorm(c(x,y), mean = c(3,-3), sigma = matrix(c(3,.2,.2,3),2,2))
  val4 <- mvtnorm::dmvnorm(c(x,y), mean = c(2,-2), sigma = matrix(c(3,-2,-2,3),2,2))
  val5x <- stats::dnorm(x, mean = -2)
  val5y <- stats::dnorm(x^2-3-2*y)

  0.4*val1 + 2*val2 + 0.3*val3 + 0.3*val4 + 0.5*val5x*val5y
}

xlim <- c(-5, 5); ylim <- c(-5, 5)
mat <- create_prob_grid(xlim, ylim, density_func)

plot_grid(mat, asp = T)
set.seed(5)
dat <- sample_from_grid(mat, n = 50)
graphics::points(dat, pch = 16, col = rgb(0,0,0,0.5))
res <- ols(dat)
plot_ols(res, xlim, lwd = 4, lty = 4, col = "blue")

pop_ols <- population_ols(mat, betalim = c(-1,1))
plot_ols(pop_ols, xlim, lwd = 4, lty = 4)

population_ols_kl <- population_ols(mat, betalim = c(-1,1))

reg <- population_regression(mat)
graphics::lines(reg[,1], reg[,2], lwd = 4)

sandwich_var <- sandwich_variance(res, mat)
sandwich_ci <- c(res - 1.96*sqrt(sandwich_var), res + 1.96*sqrt(sandwich_var))

fixed_var <- fixed_variance(mat)
fixed_ci <- c(res - 1.96*sqrt(fixed_var), res + 1.96*sqrt(fixed_var))

##########
#checking to see if the errors are uncorrelated
error_correlation(mat)

#########
# do a example where the model is correct


#########
# maybe do simulations over randomly generated correct and incorrect models?
