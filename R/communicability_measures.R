##' Criticality of the vertices
##'
##' The criticality of a vertex measures its impact
##' on its neighbors if it defaults.  It is basically the \code{\link{rowSums}}
##' of the \code{\link{impact_matrix}}.
##'
##' @inheritParams risk_matrix
##'
##' @return The function returns a (named) vector with the criticality for each vertex.
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
##' # Criticality
##' criticality(assets_matrix, buffer)
##'
##' @export
criticality <- function(exposures,
                        buffer,
                        binary = FALSE,
                        exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  v <- impact_matrix(exposures,
                     buffer,
                     binary = binary,
                     exposure_type = exposure_type)
  # caps
  v[v > 1] <- 1
  
  rowSums(v)
  
}


# Communicability --------------------------------------------------------



##' Computes the communicability matrix
##'
##' The communicability of an adjacency matrix M is defined as exp(M) where
##' M[i,j] can be interpreted as the weighted sums of paths from i to j. 
##' Recall that exp(M) can be cast into a Taylor series expansion with an 
##' infinite number additive terms. 
##' The function permits the evaluation of exp(M) using the \code{\link{expm}} package 
##' or using a simpler mathematical approximation. 
##' In the second case, the function truncates the infinite series by 
##' simply calculating the summation terms up to a pre-defined number of factors.
##' 
##'
##' @param x a square \code{\link{matrix}} or an \code{\link[igraph]{igraph}} object.
##'
##' @param terms truncates the communicability matrix evaluation up to a pre-defined number of terms.
##' If \code{terms = Inf} the function computes the matrix exponential using \code{\link{expm}}.
##'
##' @param sparse should the function use sparse matrices when computing the communicability? 
##' However, if \code{terms = Inf}
##' the function will use \code{\link{expm}} which uses \code{\link{dgeMatrix-class}}.
##'
##' @return The function returns the communicability matrix.
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
##' # Computing vulnerability
##' v <- vulnerability_matrix(assets_matrix, buffer, binary = TRUE)
##'
##' # Computing communicability of the vulnerability matrix
##' communicability_matrix(v)
##'
##' @references
##' Estrada, E. Hatano, N. (2008). Communicability in complex networks.
##' Physical Review E, 77:036111.
##'
##' @export
##'
##'@import Matrix 
communicability_matrix <- function(x,
                                   terms = Inf,
                                   sparse = TRUE){
  UseMethod("communicability_matrix")
}


##' @export
communicability_matrix.igraph <- function(x,
                                          terms = Inf,
                                          sparse = TRUE){
  x <- x[,]
  communicability_matrix(x = x,
                         terms = terms,
                         sparse = sparse)
}

##' @export
communicability_matrix.default <- function(x,
                                           terms = Inf,
                                           sparse = TRUE){
  
  if (is.infinite(terms)) {
    return(expm(x))
  }
  
  if (sparse) {
    id <- Matrix(0, nrow = nrow(x), ncol = ncol(x))
    diag(id) <- 1
  }else {
    id <- diag(1, nrow = nrow(x), ncol = ncol(x))
  }
  
  c <- id
  aux <- id
  for (i in 1:terms) {
    aux <- aux %*% x
    c <- c + aux / factorial(i)
  }
  
  rownames(c) <- rownames(x)
  colnames(c) <- colnames(x)
  return(c)
}



# Suscepibility, Fluidity and Diffusion -----------------------------------


##' Impact Susceptibility, Fluidity and Diffusion
##'
##' @description
##' The \code{impact_susceptibility} measures the
##' feasible contagion paths that can reach a vertex in relation to its
##' direct contagion paths. When the impact susceptibility is greater than 1,
##' it means that the vertex is vulnerable to other vertices beyond its direct
##' neighbors (remotely vulnerable).
##'
##' The \code{impact_fluidity} is simply the average of the impact susceptibility in
##' the network.
##'
##' The \code{impact_diffusion} tries to capture the influence
##' exercised by a node on the propagation of impacts in the network. The
##' impact diffusion of a vertex is measured by the change it causes on the
##' impact susceptibility of other vertices when its power to
##' propagate contagion is removed from the network.
##'
##' All these measures are based on the communicability of the
##' vulnerability matrix (see \code{\link{vulnerability_matrix}} and
##' \code{\link{communicability_matrix}}).
##'
##' @inheritParams risk_matrix
##' @inheritParams communicability_matrix
##'
##' @param weights default is \code{NULL}. You can use a numeric
##' vector of weights to give some economic significance to the measures, like,
##' for instance, the total assets of the nodes.
##'
##'
##' @return The \code{impact_susceptibility} function returns a vector with the (weighted) impact susceptibility
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
##' # Measures
##' impact_susceptibility(assets_matrix, buffer)
##' impact_fluidity(assets_matrix, buffer)
##' impact_diffusion(assets_matrix, buffer)
##'
##' @references
##'
##' Silva, T.C.; Souza, S.R.S.; Tabak, B.M. (2015) Monitoring vulnerability and impact
##' diffusion in financial networks. Working Paper 392, Central Bank of Brazil.
##'
##' Silva, T.C.; Souza, S.R.S.; Tabak, B.M. (2015) Network structure analysis
##' of the Brazilian interbank market . Working Paper 391, Central Bank of Brazil.
##'
##' @export
impact_susceptibility <- function(exposures,
                                  buffer,
                                  weights = NULL,
                                  terms = Inf,
                                  sparse = TRUE,
                                  binary = TRUE,
                                  exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  v <- impact_matrix(exposures,
                     buffer,
                     binary = binary,
                     exposure_type = exposure_type)
  # caps
  v[v > 1] <- 1
  
  c <- communicability_matrix(v, terms = terms, sparse = sparse)
  diag(c) <- 0
  
  if (is.null(weights)) {
    in_degree <- colSums(v)
    zeros <- which(in_degree == 0)
    s <- colSums(c)/in_degree
    s[zeros] <- 0
  }else{
    s <- colSums(c)*weights
  }
  names(s) <- colnames(v)
  return(s)
}




##' @return The \code{impact_fluidity} function returns a vector with the (weighted) impact fluidity of the network.
##' @name impact_susceptibility
##' @export
impact_fluidity <- function(exposures,
                            buffer,
                            weights = NULL,
                            terms = Inf,
                            sparse = TRUE,
                            binary = TRUE,
                            exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  s <- impact_susceptibility(exposures = exposures,
                             buffer = buffer,
                             weights = weights,
                             terms = terms,
                             sparse = sparse,
                             binary = binary,
                             exposure_type = exposure_type)
  mean(s)
}




##' @return The \code{impact_diffusion} function returns a \code{\link{data.frame}} with
##' the vertex name and the (weighted) start, intermediate and total impact diffusion.
##' @name impact_susceptibility
##' @export
impact_diffusion <- function(exposures,
                             buffer,
                             weights = NULL,
                             terms = Inf,
                             sparse = TRUE,
                             binary = TRUE,
                             exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  v <- impact_matrix(exposures,
                     buffer,
                     binary = binary,
                     exposure_type = exposure_type)
  # caps
  v[v > 1] <- 1
  
  c <- communicability_matrix(v, terms = terms, sparse = sparse)
  diag(c) <- 0
  
  start <- numeric(nrow(v))
  intermediate <- numeric(nrow(v))
  
  k <- rowSums(v)
  
  for (q in 1:nrow(v)) {
    
    if (k[q] == 0) {
      start[q] <- 0
      intermediate[q] <- 0
      next
    }
    
    v1 <- v
    v1[q, ] <- 0
    c1 <- communicability_matrix(v1, terms = terms)
    diag(c1) <- 0
    dif <- (c - c1)
    dif[dif < 0] <- 0
    if (is.null(weights)) {
      rowsums <- rowSums(dif)/k[q]
    }else{
      rowsums <- as.vector(dif %*% weights)
    }
    
    start[q] <- rowsums[q]
    intermediate[q] <- sum(rowsums[-q])
  }
  
  total <- start + intermediate
  results <- data.frame(vertex = rownames(v),
                        start,
                        intermediate,
                        total, stringsAsFactors = FALSE)
  
  return(results)
}
