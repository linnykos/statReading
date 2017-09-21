load("../main/moneyball.rda")
X <- as.matrix(moneyball[,c(4:7)]); colnames(X) <- NULL
y <- as.numeric(moneyball[,3])

y <- as.numeric(scale(y, center = T, scale = T))
X <- standardize(X)

fit <- stats::lm(y ~ X - 1)
coef1 <- stats::coef(fit)
#coef1 <- as.numeric(solve(t(X)%*%X)%*%t(X)%*%y)

d <- ncol(X); n <- nrow(X)
coef2 <- rep(NA, d)
for(i in 1:d){
  xnoti <- X[,-i]; xi <- X[,i]
  fit <- stats::lm(xi ~ xnoti)
  vec <- as.numeric(fit$residuals)
  coef2[i] <- stats::cor(y, vec)
}

all(sum(abs(coef1 - coef2)) < 1e-6)
