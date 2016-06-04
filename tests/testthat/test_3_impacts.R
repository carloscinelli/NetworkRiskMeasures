library(testthat)
context("Communicability Measures")
test_that("Simple Example",
          {
            assets_matrix <- matrix(c(0, 10, 3, 1, 0, 2, 0, 3, 0), ncol = 3)
            rownames(assets_matrix) <- colnames(assets_matrix) <- letters[1:3]
            
            ## Capital Buffer
            buffer <- c(a = 2, b = 5, c = 2)
            
            ## "Size" of the nodes
            weights <-  c(a = 10, b = 100, c = 30)
            check_is <- structure(c(0, 1, 1.25), .Names = c("a", "b", "c"))
            
            ims <- impact_susceptibility(assets_matrix, buffer)
            expect_equal(ims, check_is)
            
            imf <- impact_fluidity(assets_matrix, buffer)
            expect_equal(imf, 0.75)
            
            check_imd <- structure(list(vertex = structure(1:3, .Label = c("a", "b", "c"), class = "factor"), 
                                        start = c(1.25, 1, 0), 
                                        intermediate = c(0, 0.5, 0), 
                                        total = c(1.25, 1.5, 0)), 
                                   .Names = c("vertex", "start", "intermediate", "total"), 
                                   row.names = c(NA, -3L), class = "data.frame")
            
            imd <- impact_diffusion(assets_matrix, buffer)
            expect_equal(imd, check_imd)
          }
)