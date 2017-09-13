create_prob_grid <- function(xlim, ylim, density_func, grid_size = 200){
  xseq <- seq(xlim[1], xlim[2], length.out = grid_size)
  yseq <- seq(ylim[1], ylim[2], length.out = grid_size)

  mat <- sapply(yseq, function(y){ sapply(xseq, function(x){density_func(x,y)})})
  rownames(mat) <- yseq; colnames(mat) <- xseq
  mat
}

plot_grid <- function(mat, type = c("contour"), alpha = 0.5, nlevels = 10, ...){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))

  image(xseq, yseq, mat, col = grDevices::heat.colors(100, alpha = alpha), ...)
  contour(xseq, yseq, mat, nlevels = nlevels, add = T, ...)
}

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

#no intercept
ols <- function(dat){
  x <- dat[,1]; y <- dat[,2]

  (t(x)%*%x)^(-1)*t(x)%*%y
}

plot_ols <- function(res, xlim, ...){
  y1 <- res*xlim[1]; y2 <- res*xlim[2]
  graphics::lines(xlim, c(y1, y2), ...)
}
