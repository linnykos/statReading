rm(list=ls())
set.seed(10)

density_func_complicated_closure <- function(vec){
  stopifnot(length(vec) == 5)

  function(x,y){
    val1 <- mvtnorm::dmvnorm(c(x,y), mean = c(-2,2), sigma = 2*matrix(c(2,-1,-1,2),2,2))
    val2 <- mvtnorm::dmvnorm(c(x,y), mean = c(-1.8,1.8), sigma = 2*matrix(c(2,1,1,2),2,2))
    val3 <- mvtnorm::dmvnorm(c(x,y), mean = c(3,-3), sigma = matrix(c(3,.2,.2,3),2,2))
    val4 <- mvtnorm::dmvnorm(c(x,y), mean = c(2,-2), sigma = matrix(c(3,-2,-2,3),2,2))
    val5x <- stats::dnorm(x, mean = -2)
    val5y <- stats::dnorm(x^2-3-2*y)

    vec[1]*val1 + vec[2]*val2 + vec[3]*val3 + vec[4]*val4 + vec[5]*val5x*val5y
  }
}

density_func_linear_closure <- function(vec){
  stopifnot(length(vec) == 1)

  function(x,y){
    stats::dnorm(y-vec*x)
  }
}

simulation_function <- function(mat, n = 50){
  dat <- sample_from_grid(mat, n)
  sample_ols <- ols(dat)

  pop_ols <- population_ols(mat, betalim = c(-2,2))
  if(abs(2-pop_ols) < 1e-6 | abs(-2-pop_ols) < 1e-6) return(rep(NA, 6))

  sandwich_var <- sandwich_variance(sample_ols, dat)
  sandwich_ci <- c(sample_ols - 1.96*sqrt(sandwich_var), sample_ols + 1.96*sqrt(sandwich_var))

  fixed_var <- fixed_variance(dat)
  fixed_ci <- c(sample_ols - 1.96*sqrt(fixed_var), sample_ols + 1.96*sqrt(fixed_var))

  c(sample_ols, pop_ols, sandwich_ci, fixed_ci)
}

###########

trials <- 100
res_complicated <- sapply(1:trials, function(trial){
  if(trial %% floor(trials/10) == 0) cat('*')
  set.seed(10*trial)
  vec <- stats::rnorm(5); vec <- abs(vec)/sum(abs(vec))*10

  density_func <- density_func_complicated_closure(vec)

  xlim <- c(-5, 5); ylim <- c(-5, 5)
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 100, cores = 3)

  res <- simulation_function(mat)
  if(any(is.na(res))) print(paste0("Error in trial ", trial))
  res
})

cat("\nDone with complicated\n")
res_linear <- sapply(1:trials, function(trial){
  if(trial %% floor(trials/10) == 0) cat('*')
  set.seed(10*trial)
  vec <- stats::runif(1, min = -1.5, max = 1.5)

  density_func <- density_func_linear_closure(vec)

  xlim <- c(-5, 5); ylim <- c(-5, 5)
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 100, cores = 3)

  res <- simulation_function(mat)
  if(any(is.na(res))) print(paste0("Error in trial ", trial))
  res
})

save.image("../main/regression_ci_results.RData")
