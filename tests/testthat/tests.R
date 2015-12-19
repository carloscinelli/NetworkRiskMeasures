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
