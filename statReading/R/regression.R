#create a density over a grid
create_prob_grid <- function(xlim, ylim, density_func, grid_size = 200){
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
    y <- floor(i/h)
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
