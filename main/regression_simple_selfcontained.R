rm(list=ls())
#create a density over a grid
create_prob_grid <- function(xlim, ylim, density_func, grid_size = 100){
  xseq <- seq(xlim[1], xlim[2], length.out = grid_size)
  yseq <- seq(ylim[1], ylim[2], length.out = grid_size)

  mat <- sapply(yseq, function(y){ sapply(xseq, function(x){density_func(x,y)})})

  rownames(mat) <- yseq; colnames(mat) <- xseq
  mat/sum(mat)
}

#plot the density
plot_grid <- function(mat, type = c("contour"), alpha = 0.5, nlevels = 10, ...){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))

  image(xseq, yseq, mat, col = grDevices::heat.colors(100, alpha = alpha), ...)
  contour(xseq, yseq, mat, nlevels = nlevels, add = T, drawlabels = F, ...)
}

#sample from the density grid
sample_from_grid <- function(mat, n = 50){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))
  h <- nrow(mat); w <- ncol(mat)
  vec <- as.numeric(mat); len <- length(vec)
  idx <- sample(1:len, n, prob = vec, replace = T)

  dat <- sapply(idx, function(i){
    x <- ifelse(i %% h == 0, h, i %% h)
    y <- ceiling(i/h)
    c(xseq[x],yseq[y])
  })

  t(dat)
}

#compute the OLS coefficient (no intercept)
ols <- function(dat){
  x <- dat[,1]; y <- dat[,2]

  as.numeric((t(x)%*%x)^(-1)*t(x)%*%y)
}

#add the OLS regression line to plot
plot_ols <- function(res, xlim, ...){
  y1 <- res*xlim[1]; y2 <- res*xlim[2]
  graphics::lines(xlim, c(y1, y2), ...)
}

#compute the population OLS coefficent based on the grid, minimizing expected risk
population_ols <- function(mat, betalim, spacing = 500){
  betaseq <- seq(betalim[1], betalim[2], length.out = spacing)
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))
  vec <- as.numeric(mat)
  grid <- expand.grid(xseq, yseq)

  risk_vec <- sapply(betaseq, function(beta){
    (grid[,1]*beta - grid[,2])^2 %*% vec
  })

  idx <- which.min(risk_vec)
  betaseq[idx]
}

population_regression <- function(mat){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))
  yvec <- sapply(1:length(xseq), function(i){
    yseq%*%mat[i,]/sum(mat[i,])
  })

  cbind(xseq, yvec)
}

##################

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

grDevices::pdf("figure.pdf", 3, 3)
graphics::par(mar = rep(0, 4))
plot_grid(mat, asp = T, xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
set.seed(5)
dat <- sample_from_grid(mat, n = 50)
graphics::points(dat, pch = 16, col = rgb(0,0,0,0.5))
res <- ols(dat)
plot_ols(res, xlim, lwd = 4, lty = 4, col = "blue")

pop_ols <- population_ols(mat, betalim = c(-1,1))
plot_ols(pop_ols, xlim, lwd = 4, lty = 4)

reg <- population_regression(mat)
graphics::lines(reg[,1], reg[,2], lwd = 4, col = 3)
grDevices::graphics.off()

