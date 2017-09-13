create_prob_grid <- function(xlim, ylim, density_func, grid_size = 200){
  xseq <- seq(xlim[1], xlim[2], length.out = grid_size)
  yseq <- seq(ylim[1], ylim[2], length.out = grid_size)

  mat <- sapply(yseq, function(y){ sapply(xseq, function(x){density_func(x,y)})})
  rownames(mat) <- yseq; colnames(mat) <- xseq
  mat
}

plot_grid <- function(mat, type = c("contour"), alpha = 0.5, nlevels = 10, ...){
  xseq <- as.numeric(colnames(mat)); yseq <- as.numeric(rownames(mat))

  image(xseq, yseq, mat, col = grDevices::heat.colors(100, alpha = alpha))
  contour(xseq, yseq, mat, nlevels = nlevels, add = T, ...)
}

clockwise90 <- function(mat) { t(mat[nrow(mat):1,]) }
