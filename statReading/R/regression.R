#create a density over a grid
create_prob_grid <- function(xlim, ylim, density_func, grid_size = 200, cores = NA){
  xseq <- seq(xlim[1], xlim[2], length.out = grid_size)
  yseq <- seq(ylim[1], ylim[2], length.out = grid_size)

  if(is.na(cores)){
    mat <- sapply(yseq, function(y){ sapply(xseq, function(x){density_func(x,y)})})
  } else {
    doMC::registerDoMC(cores = cores)
    grid <- expand.grid(xseq, yseq)
    func <- function(i){density_func(grid[i,1], grid[i,2])}

    val <- unlist(foreach::"%dopar%"(foreach::foreach(i = 1:nrow(grid)),
                              func(i)))
    mat <- matrix(val, nrow = length(yseq), ncol = length(xseq))
  }

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

population_regression <- function(mat){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))
  yvec <- sapply(1:length(xseq), function(i){
    yseq%*%mat[,i]/sum(mat[,i])
  })

  cbind(xseq, yvec)
}

#assumes population sd is 1
population_ols_kl <- function(mat, betalim, spacing = 500){
  betaseq <- seq(betalim[1], betalim[2], length.out = spacing)
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))
  vec <- as.numeric(mat)
  grid <- expand.grid(xseq, yseq)

  risk_vec <- sapply(betaseq, function(beta){
    #compute the new grid
    mle_vec <- sapply(grid[,1]*beta - grid[,2], dnorm)
    log(vec/mle_vec)%*%vec
  })

  idx <- which.min(risk_vec)
  betaseq[idx]
}

#sandwich estimate of the variance. sigma here is the sd
sandwich_variance <- function(res, mat, sigma = 1){
  x <- dat[,1]; y <- dat[,2]
  residual <- y - res*x
  zbar <- mean(x * residual)
  numerator <- mean((x*residual - zbar)^2)/(4*sigma^4)
  denominator <- -mean(x^2)/(2*sigma^2)
  numerator/denominator^2
}

#variance of the fixed
fixed_variance <- function(mat, sigma = 1){
  x <- dat[,1]
  sigma*1/sum(x^2)
}

error_correlation <- function(mat){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))

  reg <- population_regression(mat)

  vec <- sapply(1:length(xseq), function(i){
    x <- reg[i,1]; yx <- reg[i,2]
    tmp <- sapply(1:length(yseq), function(j){
      (yseq[j] - yx)*mat[i,j]
    })
    sum(tmp)
  })
  sum(vec)
}
