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
##' @param shock_vector a numeric vector indicating the stress shock (between 0 and 1) for each vertex.
##' 
##' @param weights  This should be a vector
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
##' @return The \code{debt_rank_shock} function returns a object of class \code{DebtRankShock}, which is list containing:
##' \item{DebtRank}{a \code{data.frame} with the additional stress level and additional defaulted entities caused by the shock vector.}
##' \item{StressLevel}{a \code{data.frame} with the initial, final and additional stress level for each vertex caused
##' by the shock vector.}
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
##' debt_rank(exposures = assets_matrix, capital_buffer = buffer, weights = weights)
##'
##' # Arbitray shock -- 10% stress shock in each node
##' shock <- c(a = 0.1, b = 0.1, c = 0.1)
##' debt_rank_shock(exposures = assets_matrix, capital_buffer = buffer, weights = weights, shock_vector = shock)
##'
##' @references
##'
##' Battiston, S.; Puliga, M.; Kaushik, R.; Tasca, P.; Caldarelli, G. (2012).
##' DebtRank: Too central to fail? Financial Networks, the FED and systemic risk.
##' Scientific Reports, 2:541.
##' @import dplyr
##' @export
debt_rank_shock <- function(exposures,
                            capital_buffer,
                            weights,
                            shock_vector,
                            binary = FALSE,
                            exposure_type = c("assets", "liabilities", "vulnerability"),
                            max.it = 100,
                            abs.tol = 1e-9) {
  
  # Normalize weights
  weights <- weights/sum(weights)
  names(weights) <- NULL
  
  # Computes the vulnerability matrix
  v <- vulnerability_matrix(exposures = exposures,
                            capital_buffer = capital_buffer,
                            binary = binary,
                            exposure_type = exposure_type)
  
  # caps the shock vector (it can't be negative and it can't be larger than 1)
  # maybe throw a warning?
  shock_vector[shock_vector > 1] <- 1
  shock_vector[shock_vector < 0] <- 0
  
  # Checks shock vector size
  nvertices <- nrow(v)
  if (length(shock_vector) != nvertices)  stop("shock_vector must have the same length as the number of vertices in the vulnerability_matrix")
  results <- .debt_rank_shock(v = v,
                              shock_vector = shock_vector,
                              weights = weights,
                              max.it = max.it,
                              abs.tol = abs.tol)
  
  return(results)
}



##' @name debt_rank_shock
##' @export
debt_rank <- function(exposures,
                      capital_buffer,
                      weights,
                      binary = FALSE,
                      exposure_type = c("assets", "liabilities", "vulnerability"),
                      max.it = 100,
                      abs.tol = 1e-9){
  
  weights <- weights/sum(weights)
  names(weights) <- NULL
  
  v <- vulnerability_matrix(exposures,
                            capital_buffer,
                            binary = binary,
                            exposure_type = exposure_type)
  
  
  n <- nrow(v)
  zeroes <- numeric(n)
  
  scenarios <- setNames(vector(mode = "list", length = n), rownames(v))
  
  for (i in 1:n) {
    shock <- zeroes
    shock[i] <- 1
    scenarios[[i]] <- .debt_rank_shock(v = v,
                                       shock_vector = shock,
                                       weights = weights,
                                       max.it = max.it,
                                       abs.tol = abs.tol)
  }
  
  DebtRank <- data_frame(stressed_vertex = names(scenarios),
                         vertex_weight = weights)
  
  DebtRank <- bind_cols(DebtRank, bind_rows(lapply(scenarios, function(x) x$DebtRank)))
  
  reps <- sapply(scenarios, function(x) nrow(x$StressLevel))
  
  StressLevel <- data_frame(stressed_vertex = rep(names(scenarios), reps))
  slevel <- bind_rows(lapply(scenarios, function(x) x$StressLevel))
  StressLevel <- bind_cols(StressLevel, slevel)
  
  class(StressLevel) <- 'data.frame'
  class(DebtRank) <- 'data.frame'
  
  results <- list(DebtRank = DebtRank,
                  StressLevel = StressLevel)
  
  class(results) <- "DebtRank"
  return(results)
}


# internal function with no checks
# v has to be a vulnerability matrix
.debt_rank_shock <- function(v,
                             weights,
                             shock_vector,
                             max.it = 100,
                             abs.tol = 1e-9) {
  
  stressLevel <- shock_vector
  bankState   <- numeric(length(shock_vector))
  
  bankState[shock_vector > 0] <- 1 # 0 = Undistressed, 1 = Distressed, 2 = Inactive
  
  time <- 1
  
  while (any(bankState == 1)) {
    time <- time + 1
    
    previousDistressedBanks <- bankState == 1
    
    previousStressLevel <- stressLevel
    
    stressLevel <- stressLevel +
      c(stressLevel[previousDistressedBanks] %*% v[previousDistressedBanks, ])
    
    index <- stressLevel > 1
    
    if (any(index)) stressLevel[index] <- 1
    
    bankState[previousDistressedBanks] <- 2
    
    previousDeactivatedBanks <- bankState == 2
    
    newDistressedBanks <- stressLevel > 0 & !previousDeactivatedBanks
    
    if (any(newDistressedBanks)) bankState[newDistressedBanks] <- 1
    
    conv <- sqrt(sum((stressLevel - previousStressLevel) ^ 2))
    if (conv < abs.tol || time == max.it) break
  }
  
  StressLevel <- data_frame(vertex_name = rownames(v),
                            vertex_weight = weights,
                            initial_stress = shock_vector,
                            final_stress = stressLevel,
                            diff_stress = stressLevel - shock_vector)
  
  additional_stress <- c(StressLevel$diff_stress %*% weights)
  additional_defaults = sum(stressLevel == 1) - sum(shock_vector == 1)
  DebtRank <- data_frame(additional_stress = additional_stress,
                         additional_defaults = additional_defaults)
  
  class(StressLevel) <- 'data.frame'
  class(DebtRank) <- 'data.frame'
  
  
  results <- list(DebtRank = DebtRank,
                  StressLevel = StressLevel)
  class(results) <- "DebtRankShock"
  return(results)
}

# c method for Sparse Matrices
# work around to use c instead of as.vector with debt_rank
##' @export
c.Matrix <- function(...) as.vector(...)




##' @export
##' @method print DebtRankShock
print.DebtRankShock <- function(x, n = 5, ...){
  cat("\n")
  cat("Shock causes additional stress of", paste0(round(x$DebtRank$additional_stress, 4)*100, "%"),
      "in the network.", "\n")
  cat("Additional defaulted entities:", x$DebtRank$additional_defaults, "\n")
  cat("\nStress Levels:", "\n")
  StressLevel <- x$StressLevel
  names(StressLevel) <- c("Vertex Name", "Vertex Weight", "Initial Stress", "Final Stress", "Diff Stress")
  print(head(StressLevel, n))
  m <- nrow(StressLevel)
  if(m > n) cat("--", m - n,"rows omitted.")
  cat("\nA stress level of 1 means default.")
  cat("\n")
}

## Print methods for DebtRank
## 
## The \code{print} method for \code{DebtRank} and \code{DebtRankShock} objects.
## 
## @param x a \code{DebtRank} and \code{DebtRankShock} object.
## @param n how many rows to show. Default is 5.
## @param ... further arguments passed to or from other methods.
## @return \code{NULL}.
## @importFrom expm balance
## 
##' @export
##' @method print DebtRank
print.DebtRank <- function(x, n = 5, ...){
  DebtRank <- x$DebtRank
  DebtRank <- DebtRank[order(DebtRank$additional_stress, decreasing = TRUE), ]
  names(DebtRank) <- c("Stressed Vertex","Vertex Weight", "Additional Stress", "Number of Additional Defaults")
  cat("\nDebtRank (decreasing order):\n")
  print(head(DebtRank, n))
  m <- nrow(DebtRank)
  if(m > n) cat("--", m - n,"rows omitted.")
  cat("\n")
}

##' @export
##' @import ggplot2
plot.DebtRank <- function(x, 
                          text_nudge = 0.05, 
                          text_size = 4,
                          text_color = "black",
                          check_overlap = TRUE,
                          square = TRUE, ...
                          ){
  dr <- x$DebtRank
  
  # Base plot
  p <- ggplot(dr, aes_string("vertex_weight", 
               "additional_stress", 
               label = "stressed_vertex")) + geom_point() +
  xlab("\nRelative Weight") +
  ylab("DebtRank\n") +
  ggtitle("DebtRank vs Relative Weight\n")
  
  # square
  if (square){
    p <- p + xlim(0,1) + 
      ylim(0,1) + 
      geom_abline(intercept = 0, slope = 1, lty = 2, color = "grey") +
      coord_fixed()
  }
  
  # texts
  p <- p + geom_text(nudge_y = text_nudge, 
            check_overlap = check_overlap, 
            size = text_size,
            color = text_color)
  
  # additional defaults
  p <- p %+% aes_string(color = "additional_defaults") +
    scale_colour_gradient(name = "Additional\nDefaults", 
                          low = "green",
                          high = "red")
    
  # theme
  p <- p + theme_minimal() + 
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Times", face = "bold"), 
          axis.title = element_text(family = "Times"),
          axis.text = element_text(family = "Times"),
          legend.title = element_text(family = "Times", face = "italic"),
          legend.text = element_text(family = "Times", face = "italic"))
  return(p)
}


