##' DebtRank of the vertices
##' 
##' @description 
##' The \code{debt_rank} function computes the DebtRank, additional defaults and the
##' differences in stress levels caused by each vertex of the network
##' when considering its default.
##'
##' You can also use the auxiliary function \code{debt_rank_shock} to
##' simulate arbitrary shocks in the network.
##'
##' @inheritParams vulnerability_matrix
##'
##' @param weights default is equal weights to all vertices. This should be a vector
##' representing the relative importance of each vertex in the network.
##' If the vector does not sum to 1, it will be normalized. You should use
##' weights that have some practical significance like,
##' for instance, the total assets of the nodes in the case of a financial network.
##'
##' @param max.it the maximum number of iterations. Default is 100.
##'
##' @param abs.tol the absolute convergence tolerance. Default is 1e-9.
##'
##' @return The \code{debt_rank} function returns an object of class \code{DebtRank}, which is a list containing:
##' \item{DebtRank}{a \code{data.frame} with the additional stress level and additional defaulted entities caused by each stressed vertex.}
##' \item{StressLevel}{a \code{data.frame} with the initial, final and additional stress level for each other vertex caused
##' by the stressed vertex.}
##'
##' @examples
##' # Creating example data
##' ## Assets Matrix (bilateral exposures)
##' assets_matrix <- matrix(c(0, 10, 3, 1, 0, 2, 0, 3, 0), ncol = 3)
##' rownames(assets_matrix) <- colnames(assets_matrix) <- letters[1:3]
##'
##' ## Capital Buffer
##' buffer <- c(a = 2, b = 5, c = 2)
##'
##' ## "Size" of the nodes
##' weights <-  c(a = 10, b = 100, c = 30)
##'
##' # DebtRank - computes stress for each node considering its default
##' debt_rank(assets_matrix, buffer, weights)
##'
##' # Arbitray shock -- 10% stress shock in each node
##' shock <- c(a = 0.1, b = 0.1, c = 0.1)
##' debt_rank_shock(assets_matrix, buffer, shock, weights)
##'
##' @references
##'
##' Battiston, S.; Puliga, M.; Kaushik, R.; Tasca, P.; Caldarelli, G. (2012).
##' DebtRank: Too central to fail? Financial Networks, the FED and systemic risk.
##' Scientific Reports, 2:541.
##'
##' @export
debt_rank <- function(exposures,
                      capital_buffer,
                      weights = rep(1, nrow(exposures)),
                      binary = FALSE,
                      exposure_type = c("assets", "liabilities", "vulnerability"),
                      max.it = 100,
                      abs.tol = 1e-9){

  exposures <- exposures[,]
  exposures <- rowcolnames(exposures)
  zeroes <- numeric(nrow(exposures))
  scenarios <- list()
  for (i in 1:nrow(exposures)) {
    shock <- zeroes
    shock[i] <- 1
    scenarios[[rownames(exposures)[i]]] <- debt_rank_shock(exposures = exposures,
                                                     capital_buffer = capital_buffer,
                                                     shock_vector = shock,
                                                     weights = weights,
                                                     binary = binary,
                                                     exposure_type = exposure_type,
                                                     max.it = max.it,
                                                     abs.tol = abs.tol)
  }
  DebtRank <- data.frame(stressed_vertex = names(scenarios), stringsAsFactors = FALSE)
  DebtRank <- cbind(DebtRank, do.call("rbind", lapply(scenarios, function(x) x$DebtRank)))
  row.names(DebtRank) <- NULL

  reps <- sapply(scenarios, function(x) nrow(x$StressLevel))
  StressLevel <- data.frame(stressed_vertex = rep(names(scenarios), reps),
                            stringsAsFactors = FALSE, row.names = NULL)
  StressLevel <- cbind(StressLevel, do.call("rbind", lapply(scenarios, function(x) x$StressLevel)))
  row.names(StressLevel) <- NULL

  results <- list(DebtRank = DebtRank,
                  StressLevel = StressLevel)

  class(results) <- "DebtRank"
  return(results)
}

##' @param shock_vector a numeric vector indicating the stress shock (between 0 and 1) for each vertex.
##' @return The \code{debt_rank_shock} function returns a object of class \code{DebtRankShock}, which is list containing:
##' \item{DebtRank}{a \code{data.frame} with the additional stress level and additional defaulted entities caused by the shock vector.}
##' \item{StressLevel}{a \code{data.frame} with the initial, final and additional stress level for each vertex caused
##' by the shock vector.}
##'
##' @name debt_rank
##' @export
debt_rank_shock <- function(exposures,
                            capital_buffer,
                            shock_vector,
                            weights = rep(1, nrow(exposures)),
                            binary = FALSE,
                            exposure_type = c("assets", "liabilities", "vulnerability"),
                            max.it = 100,
                            abs.tol = 1e-9) {

  # Normalize weights
  weights <- weights/sum(weights)

  # Computes the vulnerability matrix
  vulnerability_matrix <- vulnerability_matrix(exposures = exposures,
                                               capital_buffer = capital_buffer,
                                               binary = binary,
                                               exposure_type = exposure_type)

  # caps the shock vector (it can't be negative and it can't be larger than 1)
  # maybe throw a warning?
  shock_vector[shock_vector > 1] <- 1
  shock_vector[shock_vector < 0] <- 0

  # Checks shock vector size
  nvertices <- nrow(vulnerability_matrix)
  if (length(shock_vector) != nvertices)  stop("shock_vector must have the same length as the number of vertices in the vulnerability_matrix")


  v <- t(vulnerability_matrix)

  stressLevel <- matrix(0, nrow = nvertices, ncol = max.it)
  bankState   <- matrix(0, nrow = nvertices, ncol = max.it)

  stressLevel[, 1] <- shock_vector
  bankState[shock_vector > 0, 1] <- 1 # 0 = Undistressed, 1 = Distressed, 2 = Inactive

  time <- 1
  while (any(bankState[,time] == 1)) {
    time <- time + 1

    previousDistressedBanks <- bankState[ ,time - 1] == 1

    # For some reason, sparse matrices were very inefficient here (orders of magnitude)
    v1 <- as.matrix(v[ ,previousDistressedBanks, drop = FALSE])

    stressLevel[,time] <- stressLevel[ , time - 1, drop = FALSE] +
      v1 %*%
      stressLevel[previousDistressedBanks, time - 1, drop = FALSE]

    index <- stressLevel[,time] > 1

    if (any(index)) stressLevel[index, time] <- 1

    bankState[,time] <- bankState[, time - 1]

    bankState[previousDistressedBanks, time] <- 2

    previousDeactivatedBanks <- bankState[,time] == 2

    newDistressedBanks <- stressLevel[,time] > 0 & !previousDeactivatedBanks

    if (any(newDistressedBanks)) bankState[newDistressedBanks , time] <- 1

    conv <- norm(stressLevel[,time, drop = FALSE] - stressLevel[,time - 1 , drop = FALSE],
                 "F")
    if (conv < abs.tol || time == max.it) break
  }

  StressLevel <- data.frame(vertex_name = rownames(v),
                            vertex_weight = weights,
                            initial_stress = stressLevel[,1],
                            final_stress = stressLevel[,time],
                            diff_stress = stressLevel[,time] - stressLevel[,1],
                            row.names = NULL,
                            stringsAsFactors = FALSE)

  DebtRank <- data.frame(additional_stress = c(StressLevel$diff_stress %*% weights),
                         additional_defaults = sum(StressLevel$final_stress == 1) -
                           sum(StressLevel$initial_stress == 1), row.names = NULL)

  results <- list(DebtRank = DebtRank,
                  StressLevel = StressLevel)
  class(results) <- "DebtRankShock"
  return(results)
}


##' @export
print.DebtRankShock <- function(x, n = 5, ...){
  cat("\n")
  cat("Shock causes additional stress of", paste0(round(x$DebtRank$additional_stress, 4)*100, "%"),
      "in the network.", "\n")
  cat("Additional defaulted entities:", x$DebtRank$additional_defaults, "\n")
  cat("\nStress Levels:", "\n")
  StressLevel <- x$StressLevel
  names(StressLevel) <- c("Vetex Name", "Vertex Weight", "Initial Stress", "Final Stress", "Diff Stress")
  print(head(StressLevel, n))
  m <- nrow(StressLevel)
  if(m > n) cat("--", m - n,"rows omitted.")
  cat("\nA stress level of 1 means default.")
  cat("\n")
}

##' @export
print.DebtRank <- function(x, n = 5, ...){
  DebtRank <- x$DebtRank
  DebtRank <- DebtRank[order(DebtRank$additional_stress, decreasing = TRUE), ]
  names(DebtRank) <- c("Stressed Vertex", "Additional Stress", "Number of Additional Defaults")
  cat("\nDebtRank (decreasing order):\n")
  print(head(DebtRank, n))
  m <- nrow(DebtRank)
  if(m > n) cat("--", m - n,"rows omitted.")
  cat("\n")
}



