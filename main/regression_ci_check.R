load("../main/regression_ci_results.RData")

coverage_complicated_vec <- apply(res_complicated, 2, function(x){
  bool1 <- (x[2] >= x[3] & x[2] <= x[4])
  bool2 <- (x[2] >= x[5] & x[2] <= x[6])
  c(bool1, bool2)
})
coverage_complicated <- apply(coverage_complicated_vec, 1, sum)/ncol(coverage_complicated_vec)

coverage_linear_vec <- apply(res_linear, 2, function(x){
  bool1 <- (x[2] >= x[3] & x[2] <= x[4])
  bool2 <- (x[2] >= x[5] & x[2] <= x[6])
  c(bool1, bool2)
})
coverage_linear <- apply(coverage_linear_vec, 1, sum)/ncol(coverage_linear_vec)
