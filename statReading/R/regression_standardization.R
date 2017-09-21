standardize <- function(mat){
  stopifnot(is.matrix(mat), !is.data.frame(mat))

  mat <- scale(mat, center = T, scale = F)
  d <- ncol(mat)
  for(i in 1:d){
    x <- mat[,-i]; y <- mat[,i]
    fit <- stats::lm(y ~ x)
    mat[,i] <- mat[,i]/stats::sd(fit$residuals)
  }

  mat
}
