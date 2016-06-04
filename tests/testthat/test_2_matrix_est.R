# Test Estimation Methods -------------------------------------------------

library(testthat)
context("Matrix Estimation")

test_that("Anand et al", 
          {
            L <- c(a = 4, b = 5, c = 5, d = 0, e = 0, f = 2, g = 4)
            A <- c(a = 7, b = 5, c = 3, d = 1, e = 3, f = 0, g = 1)
            
            M <- matrix_estimation(A, L, verbose = F)
            M2 <- round(M, 2)
            
            check_m <- c(0, 1.72, 0.98, 0.25, 0.75, 0, 0.3, 
                         2.53, 0, 1.06, 0.27, 0.81, 0, 0.32, 
                         2.18, 1.6, 0, 0.23, 0.7, 0, 0.28, 
                         0, 0, 0, 0, 0, 0, 0, 
                         0, 0, 0, 0, 0, 0, 0, 
                         0.74, 0.54, 0.31, 0.08, 0.24, 0, 0.09, 
                         1.55, 1.14, 0.65, 0.17, 0.5, 0, 0)
            
            expect_equal(c(M2), c(check_m))
            expect_equal(sum(rowSums(M)), sum(A))
            expect_equal(sum(colSums(M)), sum(L))
            
            check_md <- c(0,3,0,0,0,0,1,
                          3,0,2,0,0,0,0,
                          0,2,0,0,3,0,0, 
                          rep(0, 7), 
                          rep(0, 7),
                          0,0,1,1,0,0,0,
                          4,0,0,0,0,0,0)
            
            
            set.seed(192)
            md <- matrix_estimation(A, L, method = "md", verbose = F)
            
            expect_equal(c(md), c(check_md))
            expect_equal(sum(rowSums(md)), sum(A))
            expect_equal(sum(colSums(md)), sum(L))
          }
)

test_that("Capture printing",
          {
            L <- c(a = 4, b = 5, c = 5, d = 0, e = 0, f = 2, g = 4)
            A <- c(a = 7, b = 5, c = 3, d = 1, e = 3, f = 0, g = 1)
            
            print.me <- capture.output(max_ent(A, L))
            
            set.seed(192)
            print.md <- capture.output(min_dens(A, L))
          }
)
            