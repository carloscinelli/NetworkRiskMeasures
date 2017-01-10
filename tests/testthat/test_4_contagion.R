library(testthat)
context("Contagion functions")

test_that("debtrank",{
  # See the code to generate the dataset in the help files: ?sim_data.
  data("sim_data")
  head(sim_data)
  
  # seed - min. dens. estimation is stochastic
  set.seed(15) 
  
  # minimum density estimation
  # verbose = F to prevent printing
  md_mat <- matrix_estimation(sim_data$assets, sim_data$liabilities, method = "md", verbose = F)
  
  # rownames and colnames for the matrix
  rownames(md_mat) <- colnames(md_mat) <- sim_data$bank
  
  # DebtRank simulation
  contdr <- contagion(exposures = md_mat, buffer = sim_data$buffer, weights = sim_data$weights, 
                      shock = "all", method = "debtrank", verbose = F)
  b <- summary(contdr)
  expect_equal(b$summary_table[55,4], 58.4356749918197)
  
  s <- seq(0.01, 0.25, by = 0.01)
  shocks <- lapply(s, function(x) rep(x, nrow(md_mat)))
  names(shocks) <- paste(s*100, "pct shock")
  
  cont <- contagion(exposures = md_mat, buffer = sim_data$buffer, shock = shocks, weights = sim_data$weights, method = "debtrank", verbose = F)
  a <- summary(cont) 
  str(cont)
  print(cont)
  print(a)
  expect_equal(a$summary_table[23,4], 181.86206117054)
}
)
