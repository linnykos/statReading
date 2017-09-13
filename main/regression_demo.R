density_func <- function(x,y){
  stats::dnorm(x)*stats::dnorm(y)
}

xlim <- c(-5, 5); ylim <- c(-5, 5)
mat <- create_prob_grid(xlim, ylim, density_func)
plot_grid(mat)
