load("../main/moneyball.rda")
X <- as.matrix(moneyball[,c(4:7)]); colnames(X) <- NULL
y <- as.numeric(moneyball[,3])

y <- as.numeric(scale(y, center = T, scale = T))
X <- standardize(X)

fit <- stats::lm(y ~ X - 1)
coef1 <- stats::coef(fit)

d <- ncol(X)
coef2 <- rep(NA, d)
for(i in 1:d){
  xnoti <- X[,-i]; xi <- X[,i]
  fit <- stats::lm(xi ~ xnoti)
  vec <- as.numeric(fit$residuals)
  coef2[i] <- y %*% vec
}
