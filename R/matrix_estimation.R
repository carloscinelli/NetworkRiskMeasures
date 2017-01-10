
# Estimating Exposure Matrices --------------------------------------------

##' Matrix Estimation
##' 
##' @description 
##' Methods for estimating matrix entries from the marginals (row and column sums).
##' 
##' There are currently two methods implemented: Maximum Entropy (Upper 2004) and 
##' Minimum Density (Anand et al. 2015).
##' 
##' You may use the \code{matrix_estimation()} function, setting the desired \code{method}.
##' Or you may use directly the \code{max_ent()} function for maximum entropy estimation 
##' or the \code{min_dens()} function for minimum density estimation.
##' 
##' @param rowsums a numeric vector with the row sums.
##' @param colsums a numeric vector with the column sums.
##' @param method the matrix estimation method. Choose \code{"me"} for 
##' maximum entropy or \code{"md"} for minimum density.
##' @param ... further arguments passed to or from other methods.
##' @param max.it the maximum number of iterations.
##' @param abs.tol the desired accuracy.
##' @param verbose gives verbose output. Default is \code{TRUE}.
##' 
##' @return 
##' 
##' The functions return the estimated matrix.
##' 
##' @examples 
##' # Example from Anand, Craig and Von Peter (2015, p.628)
##' 
##' # Liabilities
##' L <- c(a = 4, b = 5, c = 5, d = 0, e = 0, f = 2, g = 4)
##' 
##' # Assets
##' A <- c(a = 7, b = 5, c = 3, d = 1, e = 3, f = 0, g = 1)
##' 
##' # Maximum Entropy
##' ME <- matrix_estimation(A, L, method = "me")
##' ME <- round(ME, 2)
##' 
##' # Minimum Density
##' set.seed(192)
##' MD <- matrix_estimation(A, L, method = "md")
##' 
##' @references 
##'  Upper, C. and A. Worm (2004). Estimating bilateral exposures in the german interbank market:
##'  Is there a danger of contagion? European Economic Review 48, 827-849.
##' 
##'  Anand, K., Craig, B. and G. von Peter (2015). Filling in the blanks:
##'  network structure and interbank contagion. 
##'  Quantitative Finance 15:4, 625-636.
##' 
##' @export
matrix_estimation <- function(rowsums, 
                              colsums,
                              method = c("me", "md"),
                              ...,
                              max.it = 1e5,
                              abs.tol = 1e-3,
                              verbose = TRUE){
 method <- match.arg(method)
 
 if (method == "me") {
   V <- max_ent(rowsums = rowsums, 
                colsums = colsums, 
                max.it = max.it, 
                abs.tol = abs.tol, 
                verbose = verbose)
 }
 
 if (method == "md") {
   V <- min_dens(rowsums = rowsums, 
                 colsums = colsums,
                 ...,
                 max.it = max.it, 
                 abs.tol = abs.tol, 
                 verbose = verbose)
 }
 return(V)
}


# Maximum Entroupy --------------------------------------------------------

##' @export
##' @name matrix_estimation
max_ent <- function(rowsums, 
                    colsums, 
                    max.it = 1e5,
                    abs.tol = 1e-3,
                    verbose = TRUE) {
  
  
  nrow <- length(rowsums)
  ncol <- length(colsums)
  M    <- matrix(1, nrow = nrow, ncol = ncol) - diag(rep(1, nrow))
  
  if (verbose) cat("Starting Maximum Entropy estimation.\n\n")
  
  for (i in 1:max.it) {
    
    # Scaling Rows
    rscale <- rcscale(rowsums, rowSums(M))
    M      <- diag(rscale) %*% M
    
    # Scaling Columns
    cscale <- rcscale(colsums, colSums(M))
    M      <- M %*% diag(cscale)
    
    error <- sum(abs(colsums - colSums(M))) + sum(abs(rowsums - rowSums(M)))
    
    if (verbose) cat("- Iteration number: ", i, 
                     " -- abs error: ", round(error, 4), 
                     " \n", 
                     sep = "")
    
    if (error < abs.tol) break
  }
  
  if (verbose) {
    if (i >= max.it) cat("\nMaximum number of iterations reached! Change the max.it parameter or other settings.\n")
    cat("\nMaximum Entropy estimation finished.",
        "\n * Total Number of Iterations: ", i, 
        "\n * Absolute Error: ", round(error, 4),
        " \n", 
        sep = "")
  }
  
  rownames(M) <- colnames(M) <- names(rowsums)
  return(M)
}

rcscale <- function(target, value){
  scale             <- value
  scale[scale <= 0] <- 0 
  scale[scale != 0] <- target[scale != 0]/scale[scale != 0]
  return(scale)
}


# Minimum Density ---------------------------------------------------------

##' @param c the 'cost' an extra link for the minimum density estimation. 
##' See Anand et al. (2015).
##' @param lambda you should use \code{lamda} 
##' together with \code{k}. For the first \code{k} rounds of 
##' the algorithm, the function will alocate a fraction \code{lambda} of the total,
##' thus obtaining a "low density" solution, instead of a "minimum density" solution.
##' See Anand et al. (2015).
##' @param k you should use \code{lamda} 
##' together with \code{k}. For the first \code{k} rounds of 
##' the algorithm, the function will alocate a fraction \code{lambda} of the total,
##' thus obtaining a "low density" solution, instead of a "minimum density" solution.
##' See Anand et al. (2015).
##' @param alpha weights for the row sums deviations. See Anand et al. (2015).
##' @param delta weights for the column sums deviations. See Anand et al. (2015).
##' @param theta scaling parameter. 
##' Emphasizes the weight placed on finding solutions with similar characteristics 
##' to the prior matrix. See Anand et al. (2015).
##' @param remove.prob probability to randomly remove a link during the algorithm. See Anand et al. (2015).
##' 
##' @name matrix_estimation
##' @export
min_dens <- function(rowsums, 
                     colsums,
                     c = 1, 
                     lambda = 1,
                     k = 100,
                     alpha = 1/sum(rowsums), 
                     delta = 1/sum(rowsums), 
                     theta = 1,
                     remove.prob = 0.01,
                     max.it = 1e5,
                     abs.tol = 1e-3,
                     verbose = TRUE){
  
  a = rowsums
  l = colsums
  
  if (lambda > 1 | lambda < 0) stop("lambda must be between 0 and 1")
  
  # number of vertices
  n = length(a)
  
  # initial matrix 
  X =  matrix(0, n, n)
  
  # matrix of indices
  mindex <- matrix(1:length(X), n, n)
  
  # positions vector for sampling
  mu = 1:length(X)
  
  # remove diagonal (it will be zero)
  mu = mu[mu != diag(mindex)]
  
  # sampled positions vector
  v = numeric(0)
  
  # asset and liabilities deficit
  ad = a - rowSums(X)
  ld = l - colSums(X)
  
  # 'prior' probabilities
  probs = Q(ad, ld, n)
  
  if (verbose) cat("Starting Minimum Density estimation.\n\n")
  
  for (t in 1:max.it) {
    
    if (t > k) lambda <- 1
    
    if ((runif(1) < remove.prob && t > 1 && length(v) > 0) || sum(probs[mu]) == 0) {
      
      # sample position to be removed
      ij = sample(v, 1)
      
      # check position row and column indices
      index <- which(mindex == ij, arr.ind = T)
      i <- index[1]
      j <- index[2]
      
      # sum the value back
      ad[i] = ad[i] + X[ij]
      ld[j] = ld[j] + X[ij]
      
      # remove the position value from X
      X[ij] = 0
      
      # include position back to the ones to be sampled
      mu = c(mu, ij)
      
      # remove it from the sampled
      v = v[v != ij]
      
    } else {
      
      # sample position to be filled
      ci <- sample.int(length(mu), 1, prob = probs[mu])
      
      # takes position value
      ij = mu[ci]
      
      # takes position row and column indices
      index <- which(mindex == ij, arr.ind = T)
      i <- index[1]
      j <- index[2]
      
      # adds value to X
      Xnew = X
      Xnew[ij] <- lambda*min(ad[i], ld[j])
      
      # computes new deficits
      
      ## assets
      adnew = ad 
      adnew[i] = adnew[i] - Xnew[ij]
      
      ## liabilities
      ldnew = ld 
      ldnew[j] = ldnew[j] - Xnew[ij]
      
      # checks new value function against old value function
      dif  = V(Xnew, adnew, ldnew, c = c, alpha = alpha, delta = delta) -
        V(X, ad, ld, c = c, alpha = alpha, delta = delta)
      
      comp1 = dif > 0
      comp2  = exp(theta*dif) > runif(1)
      
      if (comp1 || comp2) {
        # updates X, ad and ld
        X = Xnew
        ad = adnew
        ld = ldnew
        
        # includes the position in the sampled vector
        v = c(v, ij)
        # removes it from the vector to be sampled
        mu = mu[mu != ij]
      }
    }
    # updates probabilities
    probs = Q(ad, ld, n)
    
    if (verbose) cat("- Iteration number: ", t, 
                     " -- total alocated: ", 
                     round(100*(sum(a - ad)/sum(a)),6), 
                     " % \n", 
                     sep = "")
    
    if (sum(abs(ad) - 0) < abs.tol) break
  }
  
  
  if (verbose) {
    if (t >= max.it) cat("\nMaximum number of iterations reached! Change the max.it parameter or other settings.\n")
    cat("\nMinimum Density estimation finished.",
        "\n * Total Number of Iterations: ", t, 
        "\n * Total Alocated: ", round(100*(sum(a - ad)/sum(a)), 6), 
        " % \n", 
        sep = "")
  }
                               
  rownames(X) <- colnames(X) <- names(rowsums)
  
  return(X)
}


V = function(z, ad, ld, c = 1, alpha = 1, delta = 1) 
  -c*sum(z > 0) - sum((alpha^2)*ad) - sum((delta^2)*ld)


Q <- function(ad, ld, n){
  Q = rep.int(ad,  n)/rep(ld, each = n) 
  index <- (Q < 1 | is.na(Q)) # Q < 1/Q
  Q[index]  = (1/Q)[index] 
  Q[is.na(Q) | is.infinite(Q)] = 0
  return(Q)
}
