context("Test all the components of package")

## create_prob_grid is correct

test_that("create_prob_grid works", {
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
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 10)

  expect_true(is.matrix(mat))
  expect_true(abs(sum(mat) - 1) <= 1e-6)
  expect_true(all(mat > 0))
})

test_that("create_prob_grid works with cores", {
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
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 10)

  mat2 <- create_prob_grid(xlim, ylim, density_func, grid_size = 10, cores = 2)

  expect_true(sum(abs(mat - mat2)) < 1e-6)
})
