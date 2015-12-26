library(testthat)

test_that("vulnerability and communicability",
          {
            liabilities <- structure(c(0, 0, 0, 0, 0, 0,
                                       5, 0, 0, 0, 0, 2,
                                       0, 3, 0, 0, 2, 2,
                                       0, 0, 4, 0, 5, 0,
                                       10, 2, 0, 0, 5, 0,
                                       2, 0, 0, 0, 0, 0),
                                     .Dim = c(6L, 6L),
                                     .Dimnames = list(c("A", "B", "C", "E", "F", "D"),
                                                      c("A", "B", "C", "E", "F", "D")))
            assets <- t(liabilities)
            capital_buffer <- c(A = 10, B = 2, C = 2, E = 1, F = 10, D = 4)

            res <- structure(c(0, 0, 0, 0, 0, 0,
                               1, 0, 0, 0, 0, 1,
                               0, 1, 0, 0, 1, 1,
                               0, 0, 1, 0, 1, 0,
                               1, 0.2, 0, 0, 0.5, 0,
                               0.5, 0, 0, 0, 0, 0),
                             .Dim = c(6L, 6L),
                             .Dimnames = list(c("A", "B", "C", "E", "F", "D"),
                                              c("A","B", "C", "E", "F", "D")))

            v1 <- vulnerability_matrix(exposures = liabilities,
                                       capital_buffer = capital_buffer,
                                       binary = FALSE, exposure_type = "liabilities")
            expect_equal(v1, res)

            v2 <- vulnerability_matrix(exposures = assets,
                                       capital_buffer = capital_buffer,
                                       binary = FALSE,
                                       exposure_type = "assets")
            expect_equal(v2, res)

            v3 <- vulnerability_matrix(exposures = liabilities,
                                       capital_buffer = capital_buffer,
                                       binary = TRUE,
                                       exposure_type = "liabilities")
            expect_equal(v3, (res >= 1)*1)

            v4 <- vulnerability_matrix(exposures = assets,
                                       capital_buffer = capital_buffer,
                                       binary = TRUE,
                                       exposure_type = "assets")
            expect_equal(v4, (res >= 1)*1)

            res2 <- new("dgeMatrix",
                        x = c(1, 0, 0, 0, 0, 0, 1, 1,
                              0, 0, 0, 1, 1, 1, 1, 0, 1, 1.5, 0.833333333333333,
                              0.5, 1, 1, 1.5, 0.666666666666667, 1, 0, 0, 0,
                              1, 0, 0, 0, 0, 0, 0, 1),
                        Dim = c(6L, 6L),
                        Dimnames = list(c("A", "B", "C", "E", "F", "D"),
                                        c("A", "B", "C", "E", "F","D")),
                        factors = list())

            ## Testing igraph method only if package is installed
            if (require(igraph, quietly = TRUE) && installed.packages()["igraph","Version"] > "1.0.0") {
              g <- graph_from_adjacency_matrix(assets)
              vi <-  vulnerability_matrix(exposures = g,
                                          capital_buffer = capital_buffer,
                                          binary = FALSE,
                                          exposure_type = "assets")
              expect_equal(as.matrix(vi), res)

              vib <-  vulnerability_matrix(exposures = g,
                                          capital_buffer = capital_buffer,
                                          binary = TRUE,
                                          exposure_type = "assets")
              expect_equal(as.matrix(vib), (res >= 1)*1)
            }



            c1 <- communicability_matrix(v4)
            expect_equal(c1, res2)

            c2 <- communicability_matrix(Matrix(v4))
            expect_equal(c1, res2)

            c3 <- communicability_matrix(v4, terms = 10, sparse = F)
            expect_equal(c3, as.matrix(res2))

            ## Testing igraph method only if package is installed
            if (require(igraph, quietly = TRUE) && installed.packages()["igraph","Version"] > "1.0.0") {
              g <- graph_from_adjacency_matrix(assets)
              vib <-  vulnerability_matrix(exposures = g,
                                           capital_buffer = capital_buffer,
                                           binary = TRUE,
                                           exposure_type = "assets")
              ci <- communicability_matrix(vib)
              expect_equal(ci, res2)
            }


            }
)


test_that("DebtRank - Random Data", 
          {
            res <-
              structure(
                list(
                  DebtRank = structure(
                    list(
                      stressed_vertex = c("1",
                                          "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                      vertex_weight = c(0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0),
                      additional_stress = c(0,0.125596829283833,0,0,0.0464057085078027,0.0652369423410151,0,0.0188312338332124,0,0),
                      additional_defaults = c(0L, 0L,0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L)),
                    
                    .Names = c(
                      "stressed_vertex",
                      "vertex_weight",
                      "additional_stress",
                      "additional_defaults"
                    ),
                    
                    row.names = c(NA,-10L),
                    class = "data.frame"
                  ),
                  
                  StressLevel = structure(
                    list(
                      stressed_vertex = c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4","5","5","5","5","5","5","5","5","5","5","6","6","6","6","6","6","6","6","6","6","7","7","7","7","7","7","7","7","7","7","8","8","8","8","8","8","8","8","8","8","9","9","9","9","9","9","9","9","9","9","10","10","10","10","10","10","10","10","10","10"),
                      vertex_name = c("1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10","1","2","3","4","5","6","7","8","9","10"),
                      vertex_weight = c(0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0,0,0.47786067378296,0,0.231495669428445,0.0850901856174138,0.10797982474177,0.0511679379216082,0.0464057085078027,0,0),
                      initial_stress = c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1),
                      final_stress = c(1,0,0,0,0,0,0,0,0,0,0.209346243433055,1,0,0,0.152617341163366,0.68961163156093,0,0.822020598469199,0,0,0,0,1,0,0,0,0,0,0,0,0.379358765808802,0,0.789706497495893,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0.303571218715092,0,0,0,0.221309116869039,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0.221309116869039,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1),
                      diff_stress = c(0,0,0,0,0,0,0,0,0,0,0.209346243433055,0,0,0,0.152617341163366,0.68961163156093,0,0.822020598469199,0,0,0,0,0,0,0,0,0,0,0,0,0.379358765808802,0,0.789706497495893,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0.303571218715092,0,0,0,0.221309116869039,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.221309116869039,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
                    
                    .Names = c(
                      "stressed_vertex",
                      "vertex_name",
                      "vertex_weight",
                      "initial_stress",
                      "final_stress",
                      "diff_stress"
                    ),
                    
                    row.names = c(NA,-100L),
                    class = "data.frame"
                  )
                ),
                .Names = c("DebtRank",
                           "StressLevel"),
                class = "DebtRank"
              )
            
            set.seed(1)
            n <- 10
            exposures <- abs(rcauchy(n*n))
            exposures[exposures < 10] <- 0
            dim(exposures) <- c(n,n)
            
            buffer <- rowSums(exposures) + rnorm(n, mean(exposures), sd(exposures)*2)
            size <- colSums(exposures)*10
            
            expect_warning(test <- debt_rank(exposures, buffer, size))
            expect_equal(test, res)
            expect_warning(test2 <- debt_rank(Matrix(exposures), buffer, size))
            expect_equal(test2, res)
            test
            plot(test)
            
            res2 <- structure(list(
              DebtRank = structure(list(
                additional_stress = 0.100681040489344, 
                additional_defaults = 2L), 
                .Names = c("additional_stress", "additional_defaults"), 
                row.names = c(NA, -1L), class = "data.frame"), 
              StressLevel = structure(list(
                vertex_name = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                vertex_weight = c(0, 0.47786067378296, 0, 0.231495669428445, 0.0850901856174138, 0.10797982474177, 0.0511679379216082, 0.0464057085078027, 0, 0), 
                initial_stress = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), 
                final_stress = c(0.841464992261947, 0.5, 0.894853248747946, 0.5, 0.672313006609779, 0.844805815780465, 1, 1, 0.5, 0.5), 
                diff_stress = c(0.341464992261947, 0, 0.394853248747946, 0, 0.172313006609779, 0.344805815780465, 0.5, 0.5, 0, 0)), 
                .Names = c("vertex_name", "vertex_weight", "initial_stress", "final_stress", "diff_stress"), 
                row.names = c(NA, -10L), class = "data.frame")), .Names = c("DebtRank", "StressLevel"),
              class = "DebtRankShock")
            
            shock <- rep(0.5, n)
            expect_warning(test3 <- debt_rank_shock(exposures, buffer, size, shock))
            expect_equal(res2, test3)
            
          })