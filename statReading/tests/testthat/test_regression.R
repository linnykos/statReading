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

test_that("create_prob_grid passes a manual inspection", {
  density_func_linear_closure <- function(vec){
    stopifnot(length(vec) == 1)

    function(x,y){
      stats::dnorm(y-vec*x)
    }
  }
  density_func <- density_func_linear_closure(0.5)

  xlim <- c(-5, 5); ylim <- c(-5, 5)
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 50, cores = 3)

  manual_mat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))

  for(i in 1:length(yseq)){
    for(j in 1:length(xseq)){
      manual_mat[i,j] <- stats::dnorm(yseq[i] - xseq[j]*.5)
    }
  }
  manual_mat <- manual_mat/sum(manual_mat)

  res1 <- sum(abs(mat - manual_mat))
  res2 <- sum(abs(mat - t(manual_mat)))

  expect_true(res1 < res2)
})

##########

## population_regression is correct

test_that("population_regression works", {
  density_func_linear_closure <- function(vec){
    stopifnot(length(vec) == 1)

    function(x,y){
      stats::dnorm(y-vec*x)
    }
  }
  density_func <- density_func_linear_closure(0)

  xlim <- c(-5, 5); ylim <- c(-5, 5)
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 50, cores = 3)
  reg <- population_regression(mat)

  expect_true(all(dim(reg) == c(50, 2)))
  expect_true(is.matrix(reg))
})

test_that("population_regression computes the right curve for a linear function", {
  density_func_linear_closure <- function(vec){
    stopifnot(length(vec) == 1)

    function(x,y){
      stats::dnorm(y-vec*x)
    }
  }
  density_func <- density_func_linear_closure(0.5)

  xlim <- c(-5, 5); ylim <- c(-5, 5)
  mat <- create_prob_grid(xlim, ylim, density_func, grid_size = 50, cores = 3)
  reg <- population_regression(mat)

  expect_true(max(abs(reg[,1]*.5 - reg[,2])) < 0.1)
})

